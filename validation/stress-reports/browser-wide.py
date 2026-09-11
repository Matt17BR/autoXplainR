"""Record actual wide-report tasks before comparing with independent source answers."""
import json
import os
import time
from pathlib import Path

from playwright.sync_api import sync_playwright

root = Path(os.environ.get("AXR_STRESS_REPORTS", "~/.cache/autoxplain-stress-0.6.2/reports")).expanduser().resolve()
output = root / os.environ.get("AXR_STRESS_BROWSER_RUN", "browser-before")
output.mkdir(parents=True, exist_ok=True)
widths = [int(value) for value in os.environ.get("AXR_STRESS_WIDTHS", "100,500").split(",")]
observations = []
with sync_playwright() as p:
    browser = p.chromium.launch()
    for columns in widths:
        for mode in ("summary", "rows"):
            path = root / f"wide-{columns}" / f"{mode}.html"
            if not path.exists():
                continue
            for width in (1440, 390):
                page = browser.new_page(viewport={"width": width, "height": 1000})
                entry = {"predictors": columns, "mode": mode, "width": width, "errors": []}
                page.on("pageerror", lambda error: entry["errors"].append(str(error)))
                started = time.monotonic()
                page.goto(path.as_uri(), wait_until="load", timeout=60000)
                entry["load_seconds"] = time.monotonic() - started
                page.locator('[data-page-link="data"]').click()
                started = time.monotonic()
                if width > 640:
                    page.locator("#data-search").fill(f"sensor_{columns-1:03}")
                    page.locator(f'[data-column-name="sensor_{columns-1:03}"]').click()
                else:
                    page.select_option("#data-column-select", f"sensor_{columns-1:03}")
                entry["last_column_seconds"] = time.monotonic() - started
                entry["last_column"] = page.locator("#data-variable-title").inner_text()
                entry["last_column_summary"] = page.locator("#data-summary").inner_text()
                page.locator('[data-data-view="relationships"]').click()
                entry["target_pair_association"] = page.locator("#data-association").inner_text()
                entry["target_pair_note"] = page.locator("#data-pair-note").inner_text()
                started = time.monotonic()
                page.select_option("#data-y", f"sensor_{columns-2:03}")
                entry["arbitrary_pair_seconds"] = time.monotonic() - started
                entry["arbitrary_pair"] = page.locator("#data-pair").inner_text()
                entry["arbitrary_association"] = page.locator("#data-association").inner_text()
                entry["arbitrary_note"] = page.locator("#data-pair-note").inner_text()
                entry["population"] = page.locator("#data-population").inner_text()
                page.screenshot(path=str(output / f"wide-{columns}-{mode}-{width}-pair.png"))
                if width > 640:
                    page.locator("#data-search").fill("site")
                    page.locator('[data-column-name="site"]').click()
                else:
                    page.select_option("#data-column-select", "site")
                page.locator('[data-data-view="distribution"]').click()
                entry["site_summary"] = page.locator("#data-summary").inner_text()
                entry["document_width"] = page.evaluate("document.documentElement.scrollWidth")
                entry["dom_nodes"] = page.locator("*").count()
                page.screenshot(path=str(output / f"wide-{columns}-{mode}-{width}-categories.png"))
                entry["browser"] = browser.version
                observations.append(entry)
                page.close()
                (output / "observations.json").write_text(json.dumps(observations, indent=2) + "\n")
                print(columns, mode, width, {key: round(entry[key], 3) for key in entry if key.endswith("_seconds")}, flush=True)
    browser.close()
if any(entry["errors"] for entry in observations):
    raise RuntimeError("Browser exceptions recorded in observations.json")
