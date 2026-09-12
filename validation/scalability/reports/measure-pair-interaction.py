"""Measure an all-record filter and relationship task without changing its score population."""
import argparse
import json
from pathlib import Path
import time
from playwright.sync_api import sync_playwright
from browser_runtime import launch

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--folder", type=Path, required=True)
parser.add_argument("--browser", default="chromium")
parser.add_argument("--pair-limit", type=int, default=10000)
args = parser.parse_args()
folder = args.folder.resolve()
source = json.loads((folder / "source.json").read_text())
checks = []


def check(name, passed, observed=None):
    checks.append(dict(name=name, passed=bool(passed), observed=observed))


def settled(page):
    page.evaluate("()=>new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)))")


with sync_playwright() as runtime:
    browser = launch(runtime, args.browser)
    page = browser.new_page(viewport=dict(width=1440, height=1000))
    errors = []
    page.on("pageerror", lambda error: errors.append(str(error)))
    started = time.perf_counter()
    page.goto((folder / "report.html").as_uri())
    settled(page)
    load_seconds = time.perf_counter() - started
    scores = page.locator("[data-model-row]").all_text_contents()
    page.locator('[data-page-link="data"]').click()
    page.locator('[data-column-name="x"]').click()
    page.locator('[data-data-view="records"]').click()
    page.select_option("#data-filter-column", "x")
    page.select_option("#data-filter-op", "present")
    started = time.perf_counter()
    page.locator('#data-filter-form button[type="submit"]').click()
    settled(page)
    filter_seconds = time.perf_counter() - started
    matches = page.evaluate("AutoXplainRData.getState().matchingRows")
    check("Full matching population remains available to row filters",
          matches == source["evaluation_rows"] + source["training_rows"], matches)
    started = time.perf_counter()
    page.locator('[data-data-view="relationships"]').click()
    settled(page)
    relationship_seconds = time.perf_counter() - started
    association = page.locator("#data-association").inner_text()
    population = source["evaluation_rows"]
    count = min(population, args.pair_limit)
    check("Relationship visibly states the bounded evaluation sample and full population",
          f"{count:,} of {population:,} matching exported rows" in association, association)
    page.locator('[data-data-view="distribution"]').click()
    started = time.perf_counter()
    page.locator('[data-data-view="relationships"]').click()
    settled(page)
    revisit_seconds = time.perf_counter() - started
    check("Revisiting the pair preserves its deterministic sample", association == page.locator("#data-association").inner_text())
    check("Official model scores are unchanged", scores == page.locator("[data-model-row]").all_text_contents())
    check("The interaction produced no browser error", not errors, errors)
    observed_heap = None
    if args.browser == "chromium":
        session = page.context.new_cdp_session(page)
        session.send("Performance.enable")
        metrics = session.send("Performance.getMetrics")["metrics"]
        observed_heap = next(item["value"] for item in metrics if item["name"] == "JSHeapUsedSize")
    page.screenshot(path=str(folder / f"{args.browser}-filtered-relationship.png"), full_page=False)
    browser.close()

result = dict(
    browser=args.browser, html_bytes=(folder / "report.html").stat().st_size,
    load_seconds=load_seconds, filter_seconds=filter_seconds,
    filtered_relationship_seconds=relationship_seconds, revisit_seconds=revisit_seconds,
    observed_js_heap_bytes=observed_heap, memory_scope="One observed JS heap reading, not peak browser RSS.",
    visible_relationship=association, checks=checks, passed=all(item["passed"] for item in checks),
)
(folder / f"{args.browser}-pair-interaction.json").write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps(result, indent=2))
raise SystemExit(0 if result["passed"] else 1)
