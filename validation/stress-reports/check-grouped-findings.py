"""Follow every grouped diagnostic link to its actual model and feature."""
import json
import os
from pathlib import Path
from playwright.sync_api import sync_playwright

root = Path(os.environ.get("AXR_STRESS_REPORTS", "~/.cache/autoxplain-stress-0.6.2/reports")).expanduser()
folder = root / "pair-cases"
expected = json.loads((folder / "shuffle-findings.json").read_text())
if len(expected) < 2:
    raise RuntimeError("The fixture must contain multiple unresolved model/feature intervals")
checks = []


def check(name, passed, details=None):
    checks.append({"name": name, "passed": bool(passed), "details": details})


with sync_playwright() as p:
    browser = p.chromium.launch()
    for width in (1440, 390):
        page = browser.new_page(viewport={"width": width, "height": 1000})
        errors = []
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.goto((folder / "all.html").as_uri())
        page.locator('[data-page-link="checks"]').click()
        group = page.locator(".finding-group")
        observed = group.locator("tbody tr").evaluate_all("rows=>rows.map(row=>({model:row.querySelector('a').dataset.evidenceModel,feature:row.querySelector('a').dataset.evidenceFeature,evidence:row.cells[2].textContent}))")
        check(f"{width}/every-record-preserved", observed == expected, {"expected": expected, "observed": observed})
        check(f"{width}/no-horizontal-overflow", page.evaluate("document.documentElement.scrollWidth<=innerWidth"))
        page.screenshot(path=str(folder / f"grouped-findings-{width}.png"), full_page=True)
        for row in expected:
            link = group.locator(f'a[data-evidence-model="{row["model"]}"][data-evidence-feature="{row["feature"]}"]')
            target_id = link.get_attribute("href")[1:]
            link.click()
            state = page.evaluate("window.AutoXplainRReport.getState()")
            target = page.locator(f'[id="{target_id}"]')
            # These findings link to the exact shuffle evidence row in Methods,
            # not an effect curve. Its model/feature context must stay selected.
            check(f'{width}/link/{row["model"]}/{row["feature"]}', state["modelId"] == row["model"] and
                  state["feature"] == row["feature"] and target.is_visible() and
                  row["feature"] in target.inner_text() and
                  target.evaluate("node => node === document.activeElement"),
                  {"state": state, "target": target.inner_text()})
            page.locator('[data-page-link="checks"]').click()
        check(f"{width}/runtime", not errors, errors)
        page.close()
    browser.close()
summary = {"passed": sum(item["passed"] for item in checks), "failed": sum(not item["passed"] for item in checks), "checks": checks}
(folder / "grouped-findings.json").write_text(json.dumps(summary, indent=2)+"\n")
print(summary["passed"], "passed", summary["failed"], "failed")
if summary["failed"]:
    raise SystemExit(1)
