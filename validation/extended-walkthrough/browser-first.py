from pathlib import Path
import json
import os
from playwright.sync_api import sync_playwright
if not os.environ.get('AXR_EXTENDED_DIR'):
    raise SystemExit('Set AXR_EXTENDED_DIR to the directory containing extended.html.')
root = Path(os.environ['AXR_EXTENDED_DIR']).expanduser().resolve()
browser_options = {}
if os.environ.get('BROWSER_EXECUTABLE'):
    browser_options['executable_path'] = os.environ['BROWSER_EXECUTABLE']
observed={}
with sync_playwright() as p:
    browser=p.chromium.launch(**browser_options)
    page=browser.new_page(viewport={'width':1440,'height':1050})
    observed['errors']=[]
    page.on('pageerror',lambda error: observed['errors'].append(str(error)))
    page.goto((root/'extended.html').as_uri())
    observed['overview']=page.locator('#overview').inner_text()
    observed['models']=page.locator('[data-model-row]').evaluate_all('rows=>rows.map(row=>({id:row.dataset.modelRow,text:row.innerText}))')
    page.screenshot(path=str(root/'overview-desktop.png'),full_page=True)
    page.locator('[data-page-link="selection"]').click()
    observed['selection']=page.locator('#selection').inner_text()
    observed['families']=page.locator('#selection-family-filter option').evaluate_all('nodes=>nodes.map(node=>({id:node.value,label:node.textContent}))')
    page.screenshot(path=str(root/'selection-desktop.png'),full_page=True)
    page.locator('[data-page-link="patterns"]').click()
    options=page.locator('#feature-model-select option').evaluate_all('nodes=>nodes.map(node=>({id:node.value,label:node.textContent}))')
    observed['features']=[]
    for option in options:
        page.select_option('#feature-model-select',option['id'])
        panel=page.locator('#patterns [data-model-panel]:visible')
        observed['features'].append(dict(option,text=panel.inner_text()))
    observed['runtime']=browser.version
    browser.close()
(root/'observed-before-oracles.json').write_text(json.dumps(observed,indent=2)+'\n')
if observed['errors']:
    raise RuntimeError(observed['errors'])
print('Recorded overview, selection and all model panels. Inspect the screenshots before native checks.')
