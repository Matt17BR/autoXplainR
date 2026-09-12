"""Use high-cardinality filters and check numeric AsIs and identifier semantics."""
import argparse
import json
import math
from pathlib import Path
from playwright.sync_api import sync_playwright
from browser_runtime import launch

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--folder", type=Path, required=True)
parser.add_argument("--browsers", nargs="+", default=["chromium"])
args = parser.parse_args()
folder = args.folder.resolve()
source = json.loads((folder / "source.json").read_text())
checks = []


def check(name, passed, evidence=None):
    checks.append(dict(name=name, passed=bool(passed), evidence=evidence))


with sync_playwright() as runtime:
    for engine in args.browsers:
        browser = launch(runtime, engine)
        for width in (1440, 390):
            name = f"{engine}/{width}"
            page = browser.new_page(viewport={"width": width, "height": 950})
            errors = []
            page.on("pageerror", lambda error: errors.append(str(error)))
            page.goto((folder / "report.html").as_uri())
            page.locator('[data-page-link="data"]').click()
            page.locator('[data-data-view="records"]').click()
            page.select_option("#data-filter-column", "record_id")
            page.select_option("#data-filter-op", "in")
            check(name + ": category control bounds actual option creation", page.locator("#data-filter-levels option").count() == 200)
            check(name + ": category control discloses full search space", str(source["rows"]) in page.locator("#data-filter-level-count").inner_text().replace(",", ""))
            chosen = []
            for value in source["selected"]:
                page.locator("#data-filter-level-search").fill(value)
                chosen.append(value)
                page.select_option("#data-filter-levels", chosen)
                check(name + ": exact category can be found " + value, page.locator("#data-filter-levels").evaluate(
                    "el=>Array.from(el.selectedOptions,option=>option.value)") == chosen)
            page.locator('#data-filter-form button[type="submit"]').click()
            state = page.evaluate("AutoXplainRData.getState()")
            check(name + ": multi-category filter retains selections across searches", state["matchingRows"] == len(chosen))
            visible_keys = page.locator("#data-row-table tbody tr").evaluate_all("rows=>rows.map(row=>row.dataset.rowKey)")
            check(name + ": selected IDs correspond to distinct original source records", len(set(visible_keys)) == 2)
            inspected = []
            for index in range(len(visible_keys)):
                page.locator("#data-row-table tbody tr").nth(index).locator("button").click()
                cells = page.locator("#data-selected-row tbody tr").evaluate_all(
                    "rows=>Object.fromEntries(rows.map(row=>[row.cells[0].textContent,row.cells[1].textContent]))")
                inspected.append(cells["record_id"])
                check(name + ": category lookup preserves the source numeric value " + cells["record_id"],
                      math.isclose(float(cells["x"].replace(",", "")), source["selected_x"][cells["record_id"]],
                                   rel_tol=1e-14, abs_tol=1e-15))
            check(name + ": filtered rows contain exactly the requested original IDs", sorted(inspected) == sorted(source["selected"]))
            page.locator("#data-filter-reset").click()
            page.select_option("#data-filter-column", "x")
            page.select_option("#data-filter-op", "ge")
            page.locator("#data-filter-value").fill("2")
            page.locator('#data-filter-form button[type="submit"]').click()
            check(name + ": AsIs numbers remain usable numeric cells", page.evaluate(
                "AutoXplainRData.getState().matchingRows") == source["filtered_count"])
            page.locator("#data-filter-reset").click()
            page.locator('[data-data-view="relationships"]').click()
            if page.locator("#data-column-select").is_visible():
                page.select_option("#data-column-select", "record_id")
            else:
                page.locator('[data-column-name="record_id"]').click()
            page.select_option("#data-y", "x")
            text = page.locator("#relationships").inner_text()
            check(name + ": sampled pair scope distinguishes all-row summaries", "Sampled relationships" in text or
                  "Sampled relationships" in page.locator("#data-population").inner_text())
            check(name + ": identifiers do not claim perfect group association", "no repeated categories" in text)
            # A genuine row filter forces the browser's independently computed
            # pair path, which must apply the same identifier safeguard.
            page.locator('[data-data-view="records"]').click()
            page.select_option("#data-filter-column", "record_id")
            page.select_option("#data-filter-op", "contains")
            page.locator("#data-filter-value").fill("person_")
            page.locator('#data-filter-form button[type="submit"]').click()
            page.locator('[data-data-view="relationships"]').click()
            check(name + ": filtered pairs retain the identifier safeguard", "no repeated categories" in page.locator("#relationships").inner_text())
            check(name + ": no horizontal page overflow", page.evaluate("document.documentElement.scrollWidth<=innerWidth"))
            check(name + ": no browser errors", not errors, errors)
            page.close()
        browser.close()
result = dict(passed=all(item["passed"] for item in checks), checks=checks)
(folder / "category-checks.json").write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps(dict(passed=result["passed"], checks=len(checks), failures=[x for x in checks if not x["passed"]]), indent=2))
raise SystemExit(0 if result["passed"] else 1)
