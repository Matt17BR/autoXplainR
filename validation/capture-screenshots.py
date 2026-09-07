"""Capture the README gallery from the actual generated example report.

Run from any directory after validation/render-example.R. Requires Playwright
and Chromium; CHROME_PATH can select an existing Chrome installation.
"""

import os
from pathlib import Path

from playwright.sync_api import sync_playwright


ROOT = Path(__file__).resolve().parents[1]
REPORT = ROOT / "pkgdown/assets/model-report.html"
OUTPUT = ROOT / "man/figures"


def capture_region(page, name, top, bottom):
    """Keep page styling and surrounding space; never alter report data."""
    top = max(0, top)
    page.screenshot(
        path=str(OUTPUT / name),
        clip={"x": 0, "y": top, "width": 1100, "height": bottom - top},
        animations="disabled",
        full_page=True,
    )


with sync_playwright() as playwright:
    options = {"headless": True}
    if os.environ.get("CHROME_PATH"):
        options["executable_path"] = os.environ["CHROME_PATH"]
    browser = playwright.chromium.launch(**options)
    page = browser.new_page(
        viewport={"width": 1100, "height": 900}, device_scale_factor=1.5,
        reduced_motion="reduce"
    )
    page.goto(REPORT.as_uri(), wait_until="load")
    page.evaluate("document.fonts.ready")
    OUTPUT.mkdir(parents=True, exist_ok=True)

    overview = page.locator("#overview").bounding_box()
    capture_region(
        page, "guided-overview.png", 0,
        overview["y"] + overview["height"] + 20,
    )

    # Show scores and prediction disagreement. Resource measurements are
    # secondary details in the report, rather than the comparison headline.
    comparison = page.locator("#models").bounding_box()
    capture_region(
        page, "model-comparison.png", comparison["y"] - 20,
        comparison["y"] + comparison["height"] + 20,
    )
    patterns = page.locator("#patterns").bounding_box()
    first_effect = page.locator("#patterns .effect-card").first.bounding_box()
    capture_region(
        page, "model-patterns.png", patterns["y"] - 20,
        first_effect["y"] + first_effect["height"] + 20,
    )

    # Open the real diagnostic table so the gallery shows the completed checks.
    page.locator("#reliability details").evaluate_all(
        "elements => elements.forEach(element => element.open = true)"
    )
    reliability = page.locator("#reliability").bounding_box()
    capture_region(
        page, "explanation-reliability.png", reliability["y"] - 20,
        reliability["y"] + reliability["height"] + 20,
    )

    page.set_viewport_size({"width": 390, "height": 844})
    assert page.evaluate(
        "document.documentElement.scrollWidth <= window.innerWidth"
    ), "The example report overflows a 390px viewport"
    browser.close()

print("Captured four report screenshots; 390px overflow check passed.")
