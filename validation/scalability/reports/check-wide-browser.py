"""Exercise the actual 500-input report using independent saved-source answers."""
import argparse
import json
import math
from pathlib import Path
import re
import time
from playwright.sync_api import sync_playwright
from browser_runtime import launch

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--report", type=Path, required=True)
parser.add_argument("--oracle", type=Path, required=True)
parser.add_argument("--output", type=Path, required=True)
parser.add_argument("--browsers", nargs="+", default=["chromium", "webkit"])
args = parser.parse_args()
args.output.mkdir(parents=True, exist_ok=True)
source = json.loads(args.oracle.read_text())
checks, observations = [], []


def check(name, passed, observed=None):
    checks.append(dict(name=name, passed=bool(passed), observed=observed))


with sync_playwright() as runtime:
    for engine in args.browsers:
        browser = launch(runtime, engine)
        for width in (1440, 390):
            label = f"{engine}/{width}"
            page = browser.new_page(viewport=dict(width=width, height=1000))
            errors = []
            page.on("pageerror", lambda error: errors.append(str(error)))
            started = time.perf_counter()
            page.goto(args.report.resolve().as_uri(), timeout=30000)
            observations.append(dict(engine=engine, width=width, load_seconds=time.perf_counter()-started))
            scores = page.locator("[data-model-row]").all_text_contents()
            page.locator('[data-page-link="data"]').click()
            if width > 640:
                page.locator("#data-search").fill("sensor_499")
                page.locator('[data-column-name="sensor_499"]').click()
            else:
                page.select_option("#data-column-select", "sensor_499")
            check(label + ": a late input remains discoverable", page.locator("#data-variable-title").inner_text() == "sensor_499")
            page.locator('[data-data-view="relationships"]').click()
            page.select_option("#data-y", "sensor_498")
            text = page.locator("#data-association").inner_text()
            for split in ("training", "evaluation"):
                match = re.search(split + r": Spearman correlation \(signed\) ([-+.\deE]+) · n = ([\d,]+)", text)
                expected = source["pair"][split]
                check(label + ": arbitrary " + split + " relationship agrees with original values",
                      match is not None and int(match[2].replace(",", "")) == expected["n"] and
                      math.isclose(float(match[1]), expected["rho"], rel_tol=5e-4, abs_tol=5e-5), text)
            page.locator('[data-data-view="records"]').click()
            page.locator("#data-record-key").fill(source["record"]["key"])
            page.locator('#data-record-lookup button').click()
            check(label + ": original evaluation record is selectable", page.evaluate("AutoXplainRData.getState().selected") == source["record"]["key"])
            cells = page.locator("#data-selected-row tbody tr").evaluate_all("rows=>Object.fromEntries(rows.map(row=>[row.cells[0].textContent,row.cells[1].textContent]))")
            for name, expected in source["record"]["values"].items():
                check(label + ": exact original value for " + name,
                      math.isclose(float(cells[name].replace(",", "")), expected, rel_tol=1e-14, abs_tol=1e-15))
            check(label + ": no horizontal overflow", page.evaluate("document.documentElement.scrollWidth<=innerWidth"))
            check(label + ": official scores are unchanged", page.locator("[data-model-row]").all_text_contents() == scores)
            check(label + ": no runtime error", not errors, errors)
            page.screenshot(path=str(args.output / f"{engine}-{width}.png"), full_page=False)
            page.close()
        browser.close()
result = dict(passed=all(item["passed"] for item in checks), checks=checks, observations=observations)
(args.output / "checks.json").write_text(json.dumps(result, indent=2)+"\n")
print(json.dumps(dict(passed=result["passed"], checks=len(checks), failures=[item for item in checks if not item["passed"]]), indent=2))
raise SystemExit(0 if result["passed"] else 1)
