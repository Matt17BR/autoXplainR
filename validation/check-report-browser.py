"""Check the generated synthetic report in Chromium, including printed evidence.

Run after ``Rscript validation/render-example.R``. Requires Playwright 1.58.0,
Chromium, axe-core 4.13.0, and Poppler's pdftotext. Outputs stay outside the repo
unless --output-dir explicitly selects a repository directory.
"""

import argparse
import hashlib
import importlib.metadata
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import tempfile

from playwright.sync_api import sync_playwright


ROOT = Path(__file__).resolve().parents[1]
WIDTHS = (320, 390, 768, 1440)
REQUIRED_SECTIONS = (
    "overview", "evaluation", "models", "patterns", "reliability", "limits",
    "provenance", "uncertainty",
)
WCAG_TAGS = ("wcag2a", "wcag2aa", "wcag21a", "wcag21aa", "wcag22aa")
MIN_QUANTITATIVE_FONT_PX = 12


def parse_arguments():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "report", nargs="?", type=Path,
        default=Path(os.environ.get("REPORT_PATH", ROOT / "pkgdown/assets/model-report.html")),
    )
    parser.add_argument(
        "--output-dir", type=Path,
        default=Path(os.environ.get(
            "REPORT_BROWSER_OUTPUT", Path(tempfile.gettempdir()) / "autoxplain-report-browser"
        )),
    )
    parser.add_argument("--axe-path", type=Path, default=os.environ.get("AXE_PATH"))
    parser.add_argument("--chrome-path", default=os.environ.get("CHROME_PATH"))
    parser.add_argument("--target", default="delivery_hours")
    parser.add_argument("--units", default="hours")
    return parser.parse_args()


def check(result, name, passed, evidence=None):
    result["checks"].append({"name": name, "passed": bool(passed), "evidence": evidence})


def compact_axe(items):
    return [
        {
            "id": item["id"], "impact": item.get("impact"),
            "description": item["description"], "node_count": len(item["nodes"]),
            "nodes_omitted": max(0, len(item["nodes"]) - 10),
            "nodes": [
                {"target": node["target"], "failure": node.get("failureSummary")}
                for node in item["nodes"][:10]
            ],
        }
        for item in items
    ]


def inspect_structure(page):
    return page.evaluate("""() => {
      const ids = Array.from(document.querySelectorAll('[id]'), e => e.id);
      const counts = new Map();
      ids.forEach(id => counts.set(id, (counts.get(id) || 0) + 1));
      const links = Array.from(document.querySelectorAll('a[href]')).filter(a => {
        const url = new URL(a.href, document.URL), here = new URL(document.URL);
        return url.origin === here.origin && url.pathname === here.pathname && url.hash;
      });
      const missing = links.flatMap(a => {
        try {
          const id = decodeURIComponent(new URL(a.href).hash.slice(1));
          return document.getElementById(id) ? [] : [a.getAttribute('href')];
        } catch (_) { return [a.getAttribute('href')]; }
      });
      return {
        duplicate_ids: Array.from(counts).filter(([id, count]) => id && count > 1),
        fragment_links: links.length, missing_fragment_targets: missing,
        headings: Array.from(document.querySelectorAll('main h2'), e => e.textContent.trim()),
        target_text: document.querySelector('main')?.textContent || '',
        disclosure_count: document.querySelectorAll('details').length
      };
    }""")


def inspect_layout(page):
    return page.evaluate(r"""fontFloor => {
      const labels = Array.from(document.querySelectorAll('svg text')).flatMap(e => {
        const text = e.textContent.trim(), style = getComputedStyle(e);
        const box = e.getBoundingClientRect(), matrix = e.getScreenCTM();
        if (!/\d/.test(text) || !matrix || box.width === 0 || box.height === 0 ||
            style.visibility === 'hidden' || style.display === 'none') return [];
        return [{text, effective_font_px: parseFloat(style.fontSize) * Math.hypot(matrix.a, matrix.b),
          chart: e.closest('svg').getAttribute('aria-label')}];
      });
      return {
        viewport_width: window.innerWidth,
        document_width: Math.max(document.documentElement.scrollWidth, document.body.scrollWidth),
        quantitative_label_count: labels.length,
        minimum_quantitative_font_px: labels.length ? Math.min(...labels.map(x => x.effective_font_px)) : null,
        small_labels: labels.filter(x => x.effective_font_px < fontFloor - 0.01).slice(0, 20)
      };
    }""", MIN_QUANTITATIVE_FONT_PX)


def set_disclosures(page, opened):
    page.evaluate("value => document.querySelectorAll('details').forEach(e => e.open = value)", opened)


def inspect_keyboard_disclosures(page):
    """Use Tab, Enter and Space; native summaries must be reachable and togglable."""
    set_disclosures(page, True)
    structure = page.evaluate("""() => {
      const details = Array.from(document.querySelectorAll('details'));
      return {
        count: details.length,
        invalid: details.flatMap((e, i) => {
          const summary = e.querySelector(':scope > summary');
          return !summary || e.firstElementChild !== summary || !summary.textContent.trim() ? [i] : [];
        }),
        focusable_count: document.querySelectorAll('a[href],button,input,select,textarea,[tabindex],summary').length
      };
    }""")
    visited = {}
    # A newly loaded page begins with document focus. Three traversal lengths
    # accommodate browser chrome and scrollable regions without unbounded waits.
    for _ in range(max(100, 3 * structure["focusable_count"])):
        page.keyboard.press("Tab")
        focused = page.evaluate("""() => {
          const e = document.activeElement;
          if (!e?.matches('details > summary')) return null;
          const style = getComputedStyle(e);
          return {index: Array.from(document.querySelectorAll('details')).indexOf(e.parentElement),
            text: e.textContent.trim(), opened: e.parentElement.open,
            focus_visible: e.matches(':focus-visible') && style.outlineStyle !== 'none' && parseFloat(style.outlineWidth) > 0};
        }""")
        if focused is None or focused["index"] in visited:
            continue
        page.keyboard.press("Enter")
        after_enter = page.evaluate("document.activeElement.parentElement.open")
        page.keyboard.press("Space")
        after_space = page.evaluate("document.activeElement.parentElement.open")
        focused["enter_toggles"] = after_enter != focused["opened"]
        focused["space_restores"] = after_space == focused["opened"]
        visited[focused["index"]] = focused
        if len(visited) == structure["count"]:
            break
    return {"count": structure["count"], "invalid": structure["invalid"], "visited": list(visited.values())}


def keyboard_passes(evidence):
    return (
        evidence["count"] > 0 and not evidence["invalid"]
        and len(evidence["visited"]) == evidence["count"]
        and all(item["enter_toggles"] and item["space_restores"] and item["focus_visible"]
                for item in evidence["visited"])
    )


def load_page(context, report):
    page = context.new_page()
    page.set_default_timeout(15000)
    page.goto(report.as_uri(), wait_until="load")
    page.evaluate("document.fonts.ready")
    return page


def print_pdf(page, destination):
    page.pdf(path=str(destination), format="A4", print_background=True,
             display_header_footer=False, prefer_css_page_size=True)
    text_path = destination.with_suffix(".txt")
    subprocess.run(["pdftotext", "-layout", str(destination), str(text_path)], check=True)
    raw = text_path.read_text(encoding="utf-8")
    normalized = re.sub(r"\s+", "", raw)
    return normalized, {
        "pdf": destination.name, "text_characters": len(normalized),
        "normalized_text_sha256": hashlib.sha256(normalized.encode()).hexdigest(),
    }


def first_difference(first, second):
    index = next((i for i, pair in enumerate(zip(first, second)) if pair[0] != pair[1]),
                 min(len(first), len(second)))
    return {"position": index, "first": first[max(0, index - 50):index + 150],
            "second": second[max(0, index - 50):index + 150]}


def run_checks(args, result):
    report = args.report.resolve(strict=True)
    axe_path = args.axe_path.resolve(strict=True) if args.axe_path else None
    if axe_path is None:
        raise ValueError("Supply --axe-path or AXE_PATH for axe-core 4.13.0.")
    if shutil.which("pdftotext") is None:
        raise ValueError("Poppler's pdftotext is required for print equivalence checks.")
    version = importlib.metadata.version("playwright")
    if version != "1.58.0":
        raise ValueError(f"Expected Playwright 1.58.0; found {version}.")
    source_hash = hashlib.sha256(report.read_bytes()).hexdigest()
    result.update({"report": str(report), "report_sha256": source_hash,
                   "playwright": version, "widths": [], "print": {}})
    with sync_playwright() as playwright:
        launch = {"headless": True}
        if args.chrome_path:
            launch["executable_path"] = args.chrome_path
        browser = playwright.chromium.launch(**launch)
        result["chromium"] = browser.version
        for width in WIDTHS:
            print(f"Checking report at {width}px", flush=True)
            context = browser.new_context(viewport={"width": width, "height": 900},
                                          reduced_motion="reduce", offline=True)
            page = load_page(context, report)
            structure = inspect_structure(page)
            check(result, f"{width}: unique IDs", not structure["duplicate_ids"], structure["duplicate_ids"])
            check(result, f"{width}: fragment targets", structure["fragment_links"] > 0 and
                  not structure["missing_fragment_targets"], structure["missing_fragment_targets"])
            missing_sections = [section for section in REQUIRED_SECTIONS if page.locator(f"#{section}").count() != 1]
            check(result, f"{width}: report scope", not missing_sections and
                  args.target in structure["target_text"] and args.units in structure["target_text"], missing_sections)
            page.add_script_tag(path=str(axe_path))
            axe_version = page.evaluate("axe.version")
            check(result, f"{width}: axe version", axe_version == "4.13.0", axe_version)
            result["axe_core"] = axe_version
            width_result = {"width": width, "states": []}
            for name, opened in (("closed", False), ("open", True)):
                set_disclosures(page, opened)
                layout = inspect_layout(page)
                check(result, f"{width}/{name}: page overflow", layout["document_width"] <= width + 1, layout)
                check(result, f"{width}/{name}: readable quantitative labels",
                      layout["quantitative_label_count"] > 0 and not layout["small_labels"],
                      {"minimum_css_px": layout["minimum_quantitative_font_px"],
                       "required_css_px": MIN_QUANTITATIVE_FONT_PX, "small_labels": layout["small_labels"]})
                audit = page.evaluate("""async tags => await axe.run(document, {
                  runOnly: {type: 'tag', values: tags}, resultTypes: ['violations', 'incomplete']
                })""", list(WCAG_TAGS))
                violations = compact_axe(audit["violations"])
                check(result, f"{width}/{name}: WCAG A/AA", not violations, violations)
                width_result["states"].append({"state": name, "layout": layout,
                                               "violations": violations, "incomplete": compact_axe(audit["incomplete"])})
                page.screenshot(path=str(args.output_dir / f"screen-{width}-{name}.png"),
                                full_page=True, animations="disabled")
            result["widths"].append(width_result)
            context.close()

        for enabled in (True, False):
            label = "javascript" if enabled else "no-javascript"
            print(f"Checking keyboard disclosures: {label}", flush=True)
            context = browser.new_context(viewport={"width": 390, "height": 900},
                                          java_script_enabled=enabled, reduced_motion="reduce", offline=True)
            page = load_page(context, report)
            evidence = inspect_keyboard_disclosures(page)
            check(result, f"{label}: native keyboard disclosures", keyboard_passes(evidence), evidence)
            if not enabled:
                page.screenshot(path=str(args.output_dir / "screen-no-javascript.png"), full_page=True)
            context.close()

        print("Checking closed/open/no-JavaScript PDF text equivalence", flush=True)
        printed = {}
        for label, enabled, opened in (("closed", True, False), ("open", True, True),
                                       ("no-javascript", False, False)):
            context = browser.new_context(viewport={"width": 1440, "height": 900},
                                          java_script_enabled=enabled, reduced_motion="reduce", offline=True)
            page = load_page(context, report)
            set_disclosures(page, opened)
            printed[label], result["print"][label] = print_pdf(page, args.output_dir / f"print-{label}.pdf")
            check(result, f"print/{label}: nonempty report", len(printed[label]) > 200 and
                  args.target in printed[label], result["print"][label])
            context.close()
        for label in ("open", "no-javascript"):
            equal = printed["closed"] == printed[label]
            check(result, f"print: closed equals {label}", equal,
                  None if equal else first_difference(printed["closed"], printed[label]))
        browser.close()
    check(result, "report unchanged during validation", source_hash == hashlib.sha256(report.read_bytes()).hexdigest())


def main():
    args = parse_arguments()
    args.output_dir = args.output_dir.resolve()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    result = {"checks": [], "errors": [], "scope": {
        "fixture": "Synthetic delivery report with comparison models, effects, diagnostics and uncertainty",
        "axe_tags": list(WCAG_TAGS), "axe_incomplete": "Recorded for human review; not counted as passes",
        "no_javascript": "Renderer scripts disabled; browser evaluation only inspects or sets test state",
        "print_comparison": "pdftotext -layout output with whitespace removed",
    }}
    try:
        run_checks(args, result)
    except Exception as error:
        result["errors"].append(f"{type(error).__name__}: {error}")
    failures = [item["name"] for item in result["checks"] if not item["passed"]]
    result["passed"] = not failures and not result["errors"]
    result["failed_checks"] = failures
    (args.output_dir / "report-browser.json").write_text(json.dumps(result, indent=2), encoding="utf-8")
    print(json.dumps({"passed": result["passed"], "checks": len(result["checks"]),
                      "failed_checks": failures, "errors": result["errors"],
                      "output": str(args.output_dir)}), flush=True)
    return 0 if result["passed"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
