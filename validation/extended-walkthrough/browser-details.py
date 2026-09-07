from pathlib import Path
import json
import csv
import os
from playwright.sync_api import sync_playwright
if not os.environ.get('AXR_EXTENDED_DIR'):
    raise SystemExit('Set AXR_EXTENDED_DIR to the directory containing extended.html.')
root = Path(os.environ['AXR_EXTENDED_DIR']).expanduser().resolve()
browser_options = {}
if os.environ.get('BROWSER_EXECUTABLE'):
    browser_options['executable_path'] = os.environ['BROWSER_EXECUTABLE']
observed={'errors':[],'details':{},'predictions':{}}
with sync_playwright() as p:
    browser=p.chromium.launch(**browser_options)
    page=browser.new_page(viewport={'width':1440,'height':1050})
    page.on('pageerror',lambda error: observed['errors'].append(str(error)))
    page.goto((root/'extended.html').as_uri())
    for mid in ['main_model','boosting_model','forest_model']:
        page.locator('[data-page-link="overview"]').click()
        page.locator(f'[data-model-row="{mid}"] [data-open-spec]').click()
        dialog=page.locator('dialog[open]')
        observed['details'][mid]=dialog.inner_text()
        page.screenshot(path=str(root/f'{mid}-details.png'),full_page=True)
        page.keyboard.press('Escape')
    page.locator('[data-page-link="patterns"]').click()
    page.select_option('#feature-model-select','main_model')
    page.screenshot(path=str(root/'additive-effect-desktop.png'),full_page=True)
    panel=page.locator('#patterns [data-model-panel]:visible')
    panel.locator('.axr-chart-values:visible > summary').first.click()
    observed['effect_table']=panel.locator('.axr-chart-values:visible').first.inner_text()
    observed['effect_headings']=panel.locator('h3,h4').all_inner_texts()
    page.select_option('#feature-model-select','boosting_model')
    observed['boosting_effect']=page.locator('#patterns [data-model-panel]:visible').inner_text()
    page.screenshot(path=str(root/'boosting-effect-desktop.png'),full_page=True)
    page.select_option('#feature-model-select','forest_model')
    observed['limited_forest']=page.locator('#patterns [data-model-panel]:visible').inner_text()
    for mid in ['main_model','boosting_model','forest_model']:
        page.locator('[data-page-link="evaluation"]').click()
        page.select_option('#prediction-model-select',mid)
        panel=page.locator('#evaluation [data-model-panel]:visible')
        observed['predictions'][mid]={'diagnostics':panel.inner_text()}
        panel.locator('summary',has_text='Use this fitted model in R').click()
        observed['predictions'][mid]['r_code']=panel.locator('[data-prediction-code]').inner_text()
        panel.locator('.prediction-records > summary').click()
        observed['predictions'][mid]['first_case']=panel.locator('.prediction-records table tbody tr').first.inner_text()
        link=panel.locator('.prediction-records [data-select-row]').first
        observed['predictions'][mid]['source_key']=link.get_attribute('data-select-row')
        if mid=='boosting_model':
            page.screenshot(path=str(root/'boosting-predictions-desktop.png'),full_page=True)
            link.click()
            observed['linked_data']=page.locator('#data').inner_text()
            page.screenshot(path=str(root/'boosting-error-source.png'),full_page=True)
    page.set_viewport_size({'width':390,'height':850})
    page.locator('[data-page-link="patterns"]').click()
    page.select_option('#feature-model-select','boosting_model')
    page.screenshot(path=str(root/'boosting-effect-mobile.png'),full_page=True)
    observed['mobile_features']=page.locator('#patterns').inner_text()
    observed['mobile_page_overflow']=page.evaluate('document.documentElement.scrollWidth > innerWidth')
    page.locator('[data-page-link="overview"]').click()
    page.screenshot(path=str(root/'overview-mobile.png'),full_page=True)
    observed['mobile_overview']=page.locator('#overview').inner_text()
    browser.close()
(root/'detail-observations-before-oracles.json').write_text(json.dumps(observed,indent=2)+'\n')
print('Recorded optional-model details, effect, diagnostics, linked source and copied R code before native checks.')
# These are the visible table values collected in the reviewed workflow,
# not values read from the report payload or the retained R result.
rows = [line.split('\t') for line in observed['effect_table'].splitlines()
        if line.startswith('Generalized additive model\t')]
with (root / 'observed-gam-curve.csv').open('w', newline='') as output:
    writer = csv.writer(output)
    writer.writerow(['model', 'load_tonnes', 'effect', 'std_error', 'conf_low', 'conf_high', 'n', 'support'])
    writer.writerows(rows)
if observed['errors']:
    raise RuntimeError(observed['errors'])
if observed['mobile_page_overflow']:
    raise RuntimeError('The inspected mobile feature view overflows the page.')
