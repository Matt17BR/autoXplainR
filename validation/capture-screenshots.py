"""Capture actual report tabs for the README after render-example.R."""
import os
from pathlib import Path
from playwright.sync_api import sync_playwright

ROOT = Path(__file__).resolve().parents[1]
REPORT = ROOT / 'pkgdown/assets/model-report.html'
OUTPUT = ROOT / 'man/figures'
TABS = {
    'overview': ('guided-overview.png', 'model-comparison.png'),
    'patterns': ('model-patterns.png',),
    'relationships': ('input-relationships.png',),
    'evaluation': ('model-predictions.png',),
    'checks': ('explanation-reliability.png',),
}
with sync_playwright() as playwright:
    options = {'headless': True}
    if os.environ.get('CHROME_PATH'):
        options['executable_path'] = os.environ['CHROME_PATH']
    browser = playwright.chromium.launch(**options)
    page = browser.new_page(viewport={'width': 1440, 'height': 1000},
                            device_scale_factor=1.5, reduced_motion='reduce')
    page.goto(REPORT.as_uri(), wait_until='load')
    page.evaluate('document.fonts.ready')
    OUTPUT.mkdir(parents=True, exist_ok=True)
    for tab, names in TABS.items():
        page.locator(f'[data-page-link={tab}]').click()
        page.evaluate('scrollTo(0, 0)')
        assert page.locator('.workspace-page:visible').count() == 1
        for name in names:
            page.screenshot(path=str(OUTPUT / name), full_page=True, animations='disabled')
    page.set_viewport_size({'width': 390, 'height': 844})
    for tab in TABS:
        page.locator(f'[data-page-link={tab}]').click()
        assert page.evaluate('document.documentElement.scrollWidth <= innerWidth'), tab
    browser.close()
print('Captured five report tabs; every tab passed the 390px overflow check.')
