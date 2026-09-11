"""Check user-selected relationships against independent source observations."""
import bisect
import collections
import json
import math
import os
from pathlib import Path

from playwright.sync_api import sync_playwright

root = Path(os.environ.get("AXR_STRESS_REPORTS", "~/.cache/autoxplain-stress-0.6.2/reports")).expanduser()
folder = root / os.environ.get("AXR_STRESS_PAIR_RUN", "pair-cases")
source = json.loads((folder / "source.json").read_text())
lookup = {(row["partition"], row["source_row"]): row for row in source}
checks, observations = [], []


def check(name, passed, details=None):
    checks.append({"name": name, "passed": bool(passed), "details": details})


def rank(values):
    ordered = sorted(range(len(values)), key=values.__getitem__)
    result = [0] * len(values)
    i = 0
    while i < len(values):
        j = i + 1
        while j < len(values) and values[ordered[j]] == values[ordered[i]]:
            j += 1
        for position in ordered[i:j]:
            result[position] = (i + 1 + j) / 2
        i = j
    return result


def association(pairs):
    x, y = zip(*pairs)
    numeric_x = isinstance(x[0], (int, float))
    numeric_y = isinstance(y[0], (int, float))
    n = len(x)
    if numeric_x and numeric_y:
        a, b = rank(x), rank(y)
        mean = (n + 1) / 2
        return sum((u-mean)*(v-mean) for u, v in zip(a, b)) / math.sqrt(
            sum((u-mean)**2 for u in a) * sum((v-mean)**2 for v in b))
    if numeric_x != numeric_y:
        values, categories = (x, y) if numeric_x else (y, x)
        mean = sum(values)/n
        groups = collections.defaultdict(list)
        for key, value in zip(categories, values):
            groups[key].append(value)
        between = sum(len(items) * (sum(items)/len(items)-mean)**2 for items in groups.values())
        return math.sqrt(between/sum((value-mean)**2 for value in values))
    x_count, y_count, joint = collections.Counter(x), collections.Counter(y), collections.Counter(pairs)
    chi = 0
    for a in x_count:
        for b in y_count:
            expected = x_count[a]*y_count[b]/n
            chi += (joint[(a, b)]-expected)**2/expected
    return math.sqrt(chi/(n*min(len(x_count)-1, len(y_count)-1)))


def bin_label(value, axis):
    if axis["kind"] == "categorical":
        if value in axis["levels"]:
            return value
        return axis["labels"][axis["other_code"]-1 if value in axis["known_levels"] else axis["novel_code"]-1]
    breaks = axis["breaks"]
    index = bisect.bisect_right(breaks, value)
    if value == breaks[-1]:
        index -= 1
    return axis["labels"][index]


with sync_playwright() as p:
    browser = p.chromium.launch()
    for mode in ("all", "sampled", "summary", "limited-columns", "nonfinite-context"):
        source_file = "source-nonfinite.json" if mode == "nonfinite-context" else "source.json"
        source = json.loads((folder / source_file).read_text())
        lookup = {(row["partition"], row["source_row"]): row for row in source}
        for width in (1440, 390):
            page = browser.new_page(viewport={"width": width, "height": 1000})
            errors = []
            page.on("pageerror", lambda error: errors.append(str(error)))
            page.goto((folder / f"{mode}.html").as_uri())
            # Exported identities and bin definitions describe which source rows
            # to inspect. No aggregate count or correlation is used as an oracle.
            payload = json.loads(page.locator("#axr-data-payload").text_content())
            exported = payload.get("rows", [])
            if isinstance(exported, dict):
                exported = [exported]
            metrics = page.locator("#overview").inner_text()
            page.locator('[data-page-link="data"]').click()
            pairs = [("pair_a", "pair_b"), ("pair_a", "category_a"), ("category_a", "category_b")]
            if mode == "limited-columns":
                pairs = [("pair_a", "category_a")]
                names = page.locator("#data-y option").evaluate_all("nodes => nodes.map(node => node.value)")
                check(f"{mode}/{width}/omitted-columns-absent", set(names) == {"pair_a", "category_a", "response"}, names)
            for x, y in pairs:
                if width > 640:
                    page.locator("#data-search").fill(x)
                    page.locator(f'[data-column-name="{x}"]').click()
                else:
                    page.select_option("#data-column-select", x)
                page.locator('[data-data-view="relationships"]').click()
                page.select_option("#data-y", y)
                label = f"{mode}/{width}/{x}/{y}"
                scope = page.locator("#data-population").inner_text()
                text = page.locator("#data-association").inner_text()
                if mode == "summary":
                    check(label+"/honest-unavailable", "outside the aggregate computation budget" in page.locator("#data-pair").inner_text())
                    check(label+"/full-data-scope", scope.startswith("Full data"))
                    continue
                available = page.locator("#data-pair svg").count() > 0
                check(label+"/plot-available-without-dummy-filter", available)
                if not available:
                    observations.append({"task": label, "scope": scope, "text": page.locator("#data-pair").inner_text()})
                    continue
                if mode != "limited-columns":
                    check(label+"/export-scope", "Exported records" in scope and "training:" in scope and "evaluation:" in scope, scope)
                displayed_rows = page.locator("#data-pair-table tbody tr").evaluate_all(
                    "rows => rows.map(row => Array.from(row.cells, cell => cell.textContent))")
                observed_counts = {(split, a, b): int(count.replace(",", "")) for split, a, b, count in displayed_rows}
                expected_counts = collections.Counter()
                raw_pairs = collections.defaultdict(list)
                for row in exported:
                    original = lookup[(row["partition"], row["source_row"])]["raw"]
                    a, b = original[x], original[y]
                    if a is None or b is None:
                        continue
                    raw_pairs[row["partition"]].append((a, b))
                    axes = payload["profile"]["stages"]["raw"]["columns"]
                    expected_counts[(row["partition"], bin_label(a, axes[x]["axis"]), bin_label(b, axes[y]["axis"]))] += 1
                # The limited-column report keeps this pair's full aggregate;
                # every source row is exported, so the same independent oracle applies.
                check(label+"/exact-joint-counts", dict(expected_counts) == observed_counts,
                      {"expected_total": sum(expected_counts.values()), "observed_total": sum(observed_counts.values())})
                for split, values in raw_pairs.items():
                    expected = association(values)
                    lines = [line for line in text.splitlines() if line.startswith(split+":") and
                             ("correlation" in line.lower() or "Cramer's" in line)]
                    # Browser formatting uses four significant digits.
                    expected_text = format(expected, ".4g")
                    check(label+f"/{split}-association", any(expected_text in line and f"n = {len(values)}" in line for line in lines),
                          {"expected": expected, "text": lines})
                check(label+"/no-horizontal-overflow", page.evaluate("document.documentElement.scrollWidth") <= width)
                observations.append({"task": label, "scope": scope, "association": text})
                page.screenshot(path=str(folder / f"{mode}-{width}-{x}-{y}.png"))
            page.locator('[data-page-link="overview"]').click()
            check(f"{mode}/{width}/model-scores-unchanged", page.locator("#overview").inner_text() == metrics)
            check(f"{mode}/{width}/runtime", not errors, errors)
            page.close()
    html = (folder / "all.html").read_text()
    condition = "if (state.filters.length || (!stored && records.length))"
    check("mutation/one-target", html.count(condition) == 1)
    if html.count(condition) == 1:
        mutation = folder / "all-no-pair-fallback.html"
        mutation.write_text(html.replace(condition, "if (state.filters.length)"))
        page = browser.new_page(viewport={"width": 1440, "height": 1000})
        errors = []
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.goto(mutation.as_uri())
        page.locator('[data-page-link="data"]').click()
        page.locator("#data-search").fill("pair_a")
        page.locator('[data-column-name="pair_a"]').click()
        page.locator('[data-data-view="relationships"]').click()
        page.select_option("#data-y", "pair_b")
        check("mutation/disconnected-pair-is-rejected", page.locator("#data-pair svg").count() == 0 and
              "outside the aggregate computation budget" in page.locator("#data-pair").inner_text())
        check("mutation/no-unrelated-runtime-failure", not errors, errors)
        page.close()
    browser.close()
result = {"checks": checks, "passed": sum(item["passed"] for item in checks), "failed": sum(not item["passed"] for item in checks), "observations": observations}
(folder / "browser-pairs.json").write_text(json.dumps(result, indent=2) + "\n")
print(f"{result['passed']} passed; {result['failed']} failed")
for check in checks:
    if not check["passed"]:
        print(check)
if result["failed"]:
    raise SystemExit(1)
