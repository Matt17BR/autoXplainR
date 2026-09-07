"""Capture actual report tabs for the README after render-example.R."""
import os
from pathlib import Path
from playwright.sync_api import sync_playwright

ROOT = Path(__file__).resolve().parents[1]
REPORT = ROOT / 'pkgdown/assets/model-report.html'
OUTPUT = ROOT / 'man/figures'
TABS = {
    'overview': ('guided-overview.png', 'model-comparison.png'),
    'selection': ('model-selection.png',),
    'data': ('model-data.png',),
    'patterns': ('model-patterns.png',),
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
        if tab == 'selection':
            # Show a real parameter grid rather than the untuned linear family.
            page.locator('#selection-family-filter').select_option('tree')
        if tab == 'patterns':
            page.locator('#comparison-model-select').select_option(label='Neural network')
        if tab == 'data':
            page.locator('[data-column-name="parcel_kg"]').click()
        page.evaluate('() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)))')
        page.evaluate('scrollTo(0, 0)')
        assert page.locator('.workspace-page:visible').count() == 1
        for name in names:
            page.screenshot(path=str(OUTPUT / name), full_page=True, animations='disabled')
    page.locator('[data-page-link=overview]').click()
    tree_row = page.locator('.model-table tbody tr').filter(
        has=page.get_by_role('link', name='Decision tree', exact=True))
    tree_row.locator('[data-open-spec]').click()
    assert page.get_by_role('dialog').is_visible()
    page.screenshot(path=str(OUTPUT / 'model-details.png'), animations='disabled')
    page.keyboard.press('Escape')
    page.set_viewport_size({'width': 390, 'height': 844})
    for tab in TABS:
        page.locator(f'[data-page-link={tab}]').click()
        page.evaluate('() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)))')
        assert page.evaluate('document.documentElement.scrollWidth <= innerWidth'), tab
    browser.close()
print('Captured six report tabs and model details; every tab passed the 390px overflow check.')
