"""Check hand-specified CV arithmetic, source-to-mark mapping and actual label geometry.
Run render-selection-fixture.R first; this gate does not calculate expected values with package functions.
"""
from pathlib import Path
import argparse
import json
import math
import os
import subprocess
import xml.etree.ElementTree as ET
from playwright.sync_api import sync_playwright

parser = argparse.ArgumentParser()
parser.add_argument('--case-dir', type=Path, default=Path('/tmp/autoxplain-selection-cases'))
parser.add_argument('--output-dir', type=Path, required=True)
args = parser.parse_args()
args.output_dir.mkdir(parents=True, exist_ok=True)
checks = []
def check(name, passed, detail=None):
    checks.append(dict(check=name, passed=bool(passed), detail=detail))
def settled(page):
    page.evaluate('()=>new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)))')

# Deliberately independent constants from the fixture specification.
expected = {'linear_01': ([3.6, 3.6], 3.6), 'tree_01': ([3, 3], 3),
            'tree_02': ([3.2, 3.2], 3.2), 'neural_01': ([1, 3], math.sqrt(7)),
            'neural_02': ([], None)}
threshold = math.sqrt(7) + math.sqrt(20)/(2*math.sqrt(7))
source = json.loads((args.case_dir/'selection-source.json').read_text())
for candidate in source['candidates']:
    score = expected[candidate['configuration_id']][1]
    check('retained source score '+candidate['configuration_id'],
          candidate['cv_score'] is None if score is None else math.isclose(candidate['cv_score'], score))
with sync_playwright() as p:
    options = {'executable_path': os.environ['BROWSER_EXECUTABLE']} if os.environ.get('BROWSER_EXECUTABLE') else {}
    browser = p.chromium.launch(**options)
    for width in [1440, 390, 320]:
        page = browser.new_page(viewport={'width': width, 'height': 900})
        page.goto((args.case_dir/'selection-oracle.html').resolve().as_uri()); settled(page)
        check('actual final family is initial view '+str(width), page.input_value('#selection-family-filter') == 'tree')
        cards = page.locator('.selection-decision .metric').all_text_contents()
        check('lowest CV/policy/final cards stay distinct '+str(width),
              '2.6458' in cards[0] and '3.00 RMSE' in cards[1] and '3.20 RMSE' in cards[2], cards)
        check('fallback explanation survives '+str(width), 'failed refitting' in page.locator('.selection-reason').inner_text())
        for family in ['linear', 'tree', 'neural']:
            page.select_option('#selection-family-filter', family); settled(page)
            figure = page.locator('.selection-family:not([hidden])')
            geometry = figure.locator('svg').evaluate('''svg=>{
              const b=svg.getBoundingClientRect();
              const texts=[...svg.querySelectorAll('text')].filter(t=>getComputedStyle(t).display!=='none'&&t.textContent.trim());
              const clipped=texts.filter(t=>{const a=t.getBoundingClientRect();return a.left<b.left-1||a.right>b.right+1||a.top<b.top-1||a.bottom>b.bottom+1}).map(t=>t.textContent);
              const small=texts.filter(t=>t.getBoundingClientRect().height<12).map(t=>t.textContent);
              const overlaps=[];for(let i=0;i<texts.length;i++)for(let j=i+1;j<texts.length;j++){
                const a=texts[i].getBoundingClientRect(),c=texts[j].getBoundingClientRect();
                if(a.left<c.right&&a.right>c.left&&a.top<c.bottom&&a.bottom>c.top)overlaps.push([texts[i].textContent,texts[j].textContent]);}
              return {clipped,small,overlaps,
                ticks:[...svg.querySelectorAll('.selection-axis-tick')].filter(t=>getComputedStyle(t).display!=='none').map(t=>({value:+t.textContent,x:+t.getAttribute('x')})),
                threshold:+svg.querySelector('.selection-cutoff').getAttribute('x1'),
                rows:[...svg.querySelectorAll('.selection-plot-row')].map(r=>({
                  fold:[...r.querySelectorAll('.selection-fold-point')].map(x=>+x.getAttribute('cx')),
                  pooled:r.querySelector('.selection-score-point')?+r.querySelector('.selection-score-point').getAttribute('cx'):null}))};
            }''')
            check(f'label geometry {width} {family}', not geometry['clipped'] and not geometry['small'] and not geometry['overlaps'], geometry)
            a, b = geometry['ticks'][:2]
            def position(value):
                return a['x']+(value-a['value'])*(b['x']-a['x'])/(b['value']-a['value'])
            check(f'independent allowed-loss line {width} {family}', abs(geometry['threshold']-position(threshold))<.001)
            candidate_rows = [x for x in source['candidates'] if x['family'] == family]
            for candidate, marks in zip(candidate_rows, geometry['rows']):
                ident = candidate['configuration_id']; folds, score = expected[ident]
                check(f'independent fold dots {width} {ident}', len(marks['fold']) == len(folds) and all(abs(x-position(v))<.001 for x,v in zip(marks['fold'],folds)))
                check(f'independent pooled dot {width} {ident}', marks['pooled'] is None if score is None else abs(marks['pooled']-position(score))<.001)
                link = figure.locator('[data-selection-inspect]').nth(candidate_rows.index(candidate))
                link.click(); settled(page)
                detail = page.locator('.selection-candidate:not([hidden])')
                check(f'one exact detail opens {width} {ident}', detail.count() == 1 and ident in detail.locator('summary').inner_text())
                wanted = next(x['requested_parameters'] for x in source['folds'] if x['configuration_id'] == ident)
                values = detail.locator('.selection-settings').inner_text()
                check(f'requested controls reach detail {width} {ident}', all(str(value).lower() in values.lower() for value in wanted.values()) if isinstance(wanted,dict) else 'requested:' in values)
            check(f'no page/table overflow {width} {family}', page.evaluate('()=>document.documentElement.scrollWidth<=innerWidth') and figure.locator('.selection-candidate-table').evaluate('x=>x.scrollWidth<=x.clientWidth+1'))
            page.screenshot(path=str(args.output_dir/f'{family}-{width}.png'), full_page=True)
        page.close()
    page=browser.new_page(viewport={'width':320,'height':900},java_script_enabled=False)
    page.goto((args.case_dir/'selection-oracle.html').resolve().as_uri())
    check('no-JS mobile retains all candidate details', page.locator('.selection-candidate').count()==5)
    check('no-JS mobile explains table fallback', 'All candidate and fold numbers' in page.locator('noscript').inner_text())
    page.close()
    page=browser.new_page(viewport={'width':1440,'height':900})
    page.goto((args.case_dir/'selection-oracle.html').resolve().as_uri());settled(page)
    page.select_option('#selection-family-filter','tree');settled(page)
    page.locator('.selection-family:not([hidden]) [data-selection-inspect]').nth(1).click();settled(page)
    def selection_state():
        return page.evaluate('''()=>({family:document.querySelector('#selection-family-filter').value,
          visible:[...document.querySelectorAll('.selection-family')].filter(x=>!x.hidden).map(x=>x.dataset.selectionFamily),
          opened:[...document.querySelectorAll('.selection-candidate')].filter(x=>!x.hidden&&x.open).map(x=>x.id)})''')
    before_print=selection_state()
    page.pdf(path=str(args.output_dir/'selection.pdf'),format='A4',print_background=True)
    settled(page)
    check('print preserves chosen family and opened candidate after returning',selection_state()==before_print,before_print)
    browser.close()
# The PDF must contain the chosen tree setting's open evidence, not every family's
# hidden candidate details. Family names in the decision summary are still valid.
pdf_text=subprocess.check_output(['pdftotext',str(args.output_dir/'selection.pdf'),'-'],text=True)
pdf_text=' '.join(pdf_text.split())
check('PDF keeps the chosen family rationale', 'Decision tree: search rationale' in pdf_text and
      'Neural network: search rationale' not in pdf_text and 'Linear regression: search rationale' not in pdf_text)
check('PDF keeps only the explicitly opened fold detail', 'tree_02 fold scores' in pdf_text and
      all(ident+' fold scores' not in pdf_text for ident in ['tree_01','linear_01','neural_01','neural_02']))
# Poppler font sizes at zoom1 are PDF points, independent of screen viewBox math.
pdf_xml = subprocess.check_output(['pdftohtml', '-xml', '-stdout', '-zoom', '1',
                                   str(args.output_dir/'selection.pdf')], text=True)
root = ET.fromstring(pdf_xml)
fonts = {node.attrib['id']: float(node.attrib['size']) for node in root.iter('fontspec')}
printed = [(''.join(node.itertext()), fonts[node.attrib['font']]) for node in root.iter('text')
           if 'lower is better' in ''.join(node.itertext()) or ''.join(node.itertext()).startswith('depth ')]
check('actual printed chart text is at least8pt', len(printed)>=3 and all(size>=8 for _,size in printed), printed)
(args.output_dir/'selection-checks.json').write_text(json.dumps(checks,indent=2))
print(json.dumps({'checks':len(checks),'failures':[x for x in checks if not x['passed']]},indent=2))
raise SystemExit(0 if all(x['passed'] for x in checks) else 1)
