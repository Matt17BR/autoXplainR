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
with sync_playwright() as p:
    browser=p.chromium.launch(**browser_options)
    page=browser.new_page(viewport={'width':1440,'height':1050})
    page.goto((root/'extended.html').as_uri())
    page.locator('[data-page-link="selection"]').click()
    page.select_option('#selection-family-filter','additive')
    links=page.locator('.selection-candidate-table:visible a').evaluate_all('nodes=>nodes.map(n=>({text:n.innerText,href:n.getAttribute("href")}))')
    data={'links':links,'folds':[]}
    for link in links:
        page.locator('.selection-candidate-table:visible a[href="'+link['href']+'"]').click()
        data['folds'].append({'id':link['href'],'text':page.locator(link['href']).inner_text()})
    page.locator('summary',has_text='Exact selection arithmetic and preference order').click()
    data['text']=page.locator('#selection').inner_text()
    page.screenshot(path=str(root/'additive-folds.png'),full_page=True)
    (root/'selection-detail-observations.json').write_text(json.dumps(data,indent=2)+'\n')
    browser.close()
