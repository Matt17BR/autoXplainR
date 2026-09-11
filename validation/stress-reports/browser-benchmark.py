"""Record difficult-model report tasks and exact visible model identities."""
import json
import os
from pathlib import Path
from playwright.sync_api import sync_playwright

root = Path("~/.cache/autoxplain-stress-0.6.2/reports").expanduser()
folder = root / os.environ.get("AXR_STRESS_BENCHMARK", "sparse_wide-core")
out = folder / "browser"
out.mkdir(exist_ok=True)
observations = []
with sync_playwright() as p:
    browser = p.chromium.launch()
    for width in (1440, 390):
        page = browser.new_page(viewport={"width": width, "height": 1000})
        record = {"width": width, "errors": [], "tabs": {}}
        page.on("pageerror", lambda error: record["errors"].append(str(error)))
        page.goto((folder / "summary.html").as_uri(), timeout=60000)
        for tab in ("overview", "selection", "data", "patterns", "evaluation", "checks", "provenance"):
            page.locator(f'[data-page-link="{tab}"]').click()
            record["tabs"][tab] = page.locator(f"#{tab}").inner_text()
            page.screenshot(path=str(out / f"{width}-{tab}.png"), full_page=True)
        page.locator('[data-page-link="overview"]').click()
        record["model_rows"] = page.locator("[data-model-row]").evaluate_all(
            "rows => rows.map(row => ({id:row.dataset.modelRow, text:row.innerText}))")
        metrics = page.locator("#metric-select option").evaluate_all("nodes => nodes.map(node=>node.value)")
        for metric in metrics:
            page.select_option("#metric-select", metric)
            page.screenshot(path=str(out / f"{width}-metric-{metric}.png"), full_page=True)
        page.locator('[data-page-link="selection"]').click()
        if page.locator("#selection-family-filter").count():
            options = page.locator("#selection-family-filter option").evaluate_all("nodes=>nodes.map(node=>node.value)")
            record["family_options"] = options
            for family in options:
                page.select_option("#selection-family-filter", family)
                record["tabs"]["selection-"+family] = page.locator("#selection").inner_text()
                page.screenshot(path=str(out / f"{width}-selection-{family}.png"), full_page=True)
        record["horizontal_overflow"] = page.evaluate("document.documentElement.scrollWidth > window.innerWidth")
        observations.append(record)
        page.close()
    browser.close()
(out / "observations.json").write_text(json.dumps(observations, indent=2)+"\n")
if any(record["errors"] for record in observations):
    raise RuntimeError("Actual browser exceptions recorded")
print("Recorded all task tabs, metrics, and retained family searches at desktop and phone sizes.")
