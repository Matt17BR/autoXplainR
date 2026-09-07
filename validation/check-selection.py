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
expected_labels = {'linear_01': 'No tuned controls', 'tree_01': 'depth 2; cp 0.03; split 8',
                   'tree_02': 'depth 4; cp 0.01; split 5', 'neural_01': '1 unit; decay 0.1',
                   'neural_02': '2 units; decay 0.03'}
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
        failure_id = 'selection-detail-6e657572616c5f3032'
        page = browser.new_page(viewport={'width': width, 'height': 900})
        page.goto((args.case_dir/'selection-oracle.html').resolve().as_uri()+'#validation'); settled(page)
        check(f'failed CV link starts in Checks with a different selected family {width}',
              page.locator('#failed-cv-link').is_visible() and page.input_value('#selection-family-filter')=='tree')
        page.locator('#failed-cv-link').focus(); page.keyboard.press('Enter'); settled(page)
        failed = page.locator('#'+failure_id)
        check(f'keyboard Checks link selects the failed configuration family {width}',
              page.input_value('#selection-family-filter')=='neural' and
              page.locator('.selection-family:not([hidden])').get_attribute('data-selection-family')=='neural')
        check(f'keyboard Checks link opens and focuses the exact failed folds {width}',
              failed.is_visible() and failed.evaluate('node=>node.open') and
              page.locator('.selection-candidate:not([hidden])').count()==1 and
              failed.locator('summary').evaluate('node=>node===document.activeElement') and
              'Fixture iteration limit' in failed.inner_text())
        check(f'failed configuration destination persists in URL {width}', page.url.endswith('#'+failure_id))
        page.reload(); settled(page)
        check(f'fresh deep link restores failed family and opens exact folds {width}',
              page.input_value('#selection-family-filter')=='neural' and failed.is_visible() and
              failed.evaluate('node=>node.open') and
              failed.locator('summary').evaluate('node=>node===document.activeElement'))
        page.close()
        page = browser.new_page(viewport={'width': width, 'height': 900})
        page.goto((args.case_dir/'selection-oracle.html').resolve().as_uri()); settled(page)
        check('actual final family is initial view '+str(width), page.input_value('#selection-family-filter') == 'tree')
        cards = page.locator('.selection-decision .metric').all_text_contents()
        check('lowest CV/policy/final cards stay distinct '+str(width),
              len(cards)==3 and '2.6458' in cards[0] and '3.00 RMSE' in cards[1] and '3.20 RMSE' in cards[2], cards)
        check('fallback explanation survives '+str(width), 'failed refitting' in page.locator('.selection-reason').inner_text())
        for family in ['linear', 'tree', 'neural']:
            page.select_option('#selection-family-filter', family); settled(page)
            figure = page.locator('.selection-family:not([hidden])')
            precision = figure.locator('.selection-precision')
            check(f'candidate comparison starts with one visible view {width} {family}',
                  figure.locator('.selection-plot').is_visible() and not precision.evaluate('node=>node.open')
                  and not figure.locator('.selection-candidate-table').is_visible())
            figure.locator('.selection-chart-help .help-button').focus()
            page.keyboard.press('Tab')
            check(f'normal tab order reaches first graph row {width} {family}',
                  figure.locator('.selection-plot-link').first.evaluate('node=>document.activeElement===node'))
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
                link = figure.locator('.selection-plot-link').nth(candidate_rows.index(candidate))
                pooled_text = link.locator('.selection-score-label').text_content()
                check(f'pooled score and status visible without opening details {width} {ident}',
                      (pooled_text == 'n/a' if score is None else abs(float(pooled_text)-score)<.00006)
                      and bool(link.locator('.selection-status-label').text_content()))
                shown = link.locator('.selection-row-label tspan').all_text_contents()
                check(f'meaningful controls remain on graph row {width} {ident}',
                      ''.join(''.join(shown).split()) == ''.join(expected_labels[ident].split()), shown)
                link.focus(); page.keyboard.press('Enter'); settled(page)
                detail = page.locator('.selection-candidate:not([hidden])')
                check(f'keyboard graph row opens one exact detail {width} {ident}',
                      detail.count() == 1 and ident in detail.locator('summary').inner_text()
                      and detail.locator('summary').evaluate('node=>document.activeElement===node'))
                wanted = next(x['requested_parameters'] for x in source['folds'] if x['configuration_id'] == ident)
                values = detail.locator('.selection-settings').inner_text()
                check(f'requested controls reach detail {width} {ident}', all(str(value).lower() in values.lower() for value in wanted.values()) if isinstance(wanted,dict) else 'requested:' in values)
            precision.locator('summary').click(); settled(page)
            table_rows = figure.locator('.selection-candidate-table tbody tr')
            check(f'precision table can be opened explicitly {width} {family}',
                  figure.locator('.selection-candidate-table').is_visible() and table_rows.count()==len(candidate_rows))
            for index, candidate in enumerate(candidate_rows):
                row = table_rows.nth(index)
                ident = candidate['configuration_id']; score = expected[ident][1]
                actual = row.locator('td').nth(1).inner_text()
                check(f'precision view preserves requested tuple and pooled score {width} {ident}',
                      (candidate['hyperparameters'] in row.inner_text() if family!='linear' else 'No tuned controls' in row.inner_text())
                      and (actual=='n/a' if score is None else abs(float(actual)-score)<.00006))
                row.locator('[data-selection-inspect]').click(); settled(page)
                target = row.locator('[data-selection-inspect]').get_attribute('data-selection-inspect')
                check(f'precision link reaches same fold evidence as graph {width} {ident}',
                      page.locator('.selection-candidate:not([hidden])').get_attribute('id')==target
                      and figure.locator(f'[data-selection-inspect="{target}"]').evaluate_all('nodes=>nodes.every(node=>node.getAttribute("aria-expanded")==="true")'))
            check(f'no page/table overflow {width} {family}', page.evaluate('()=>document.documentElement.scrollWidth<=innerWidth') and figure.locator('.selection-candidate-table').evaluate('x=>x.scrollWidth<=x.clientWidth+1'))
            precision.locator('summary').click(); settled(page)
            page.screenshot(path=str(args.output_dir/f'{family}-{width}.png'), full_page=True)
        page.close()
        page = browser.new_page(viewport={'width': width, 'height': 900})
        page.goto((args.case_dir/'selection-agreement.html').resolve().as_uri()); settled(page)
        agreed = page.locator('.selection-agreed')
        check(f'identical configuration uses one compact summary {width}', agreed.count()==1
              and page.locator('.selection-decision').count()==0)
        check(f'compact summary retains score, model, parameters and agreement {width}',
              all(text in agreed.inner_text() for text in ['2.6458 RMSE', 'Neural network', '1 unit; decay 0.1',
                  'Lowest CV loss, policy choice and final fit are the same configuration']))
        check(f'agreed decision opens its actual family {width}',page.input_value('#selection-family-filter')=='neural')
        if width < 650:
            check(f'agreed summary fits a compact mobile block {width}',agreed.bounding_box()['height']<100,
                  agreed.bounding_box())
        page.screenshot(path=str(args.output_dir/f'agreement-{width}.png'),full_page=True)
        page.close()
    page=browser.new_page(viewport={'width':320,'height':900},java_script_enabled=False)
    page.goto((args.case_dir/'selection-oracle.html').resolve().as_uri())
    check('no-JS mobile retains all candidate details', page.locator('.selection-candidate').count()==5)
    check('no-JS mobile explains table fallback', 'All candidate and fold numbers' in page.locator('noscript').inner_text())
    precision=page.locator('.selection-precision').first
    precision.locator('summary').click()
    check('no-JS exact settings open natively',precision.locator('table').is_visible())
    page.close()
    page=browser.new_page(viewport={'width':1440,'height':900})
    page.goto((args.case_dir/'selection-oracle.html').resolve().as_uri());settled(page)
    page.select_option('#selection-family-filter','tree');settled(page)
    page.locator('.selection-family:not([hidden]) .selection-plot-link').nth(1).click();settled(page)
    def selection_state():
        return page.evaluate('''()=>({family:document.querySelector('#selection-family-filter').value,
          visible:[...document.querySelectorAll('.selection-family')].filter(x=>!x.hidden).map(x=>x.dataset.selectionFamily),
          opened:[...document.querySelectorAll('.selection-candidate')].filter(x=>!x.hidden&&x.open).map(x=>x.id)})''')
    before_print=selection_state()
    page.pdf(path=str(args.output_dir/'selection.pdf'),format='A4',print_background=True)
    settled(page)
    check('print preserves chosen family and opened candidate after returning',selection_state()==before_print,before_print)
    check('graph navigation is restored after printing', page.locator('.selection-plot-link').evaluate_all(
        'nodes=>nodes.every(node=>node.getAttribute("href")==="#"+node.dataset.selectionInspect)'))
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
pdf_result = subprocess.run(['pdftohtml', '-xml', '-stdout', '-zoom', '1',
                            str(args.output_dir/'selection.pdf')], text=True, capture_output=True, check=True)
check('selected family PDF has no broken named destinations',
      'Bad named destination' not in pdf_result.stderr, pdf_result.stderr)
root = ET.fromstring(pdf_result.stdout)
fonts = {node.attrib['id']: float(node.attrib['size']) for node in root.iter('fontspec')}
printed = [(''.join(node.itertext()), fonts[node.attrib['font']]) for node in root.iter('text')
           if 'lower is better' in ''.join(node.itertext()) or ''.join(node.itertext()).startswith('depth ')]
check('actual printed chart text is at least8pt', len(printed)>=3 and all(size>=8 for _,size in printed), printed)
(args.output_dir/'selection-checks.json').write_text(json.dumps(checks,indent=2))
print(json.dumps({'checks':len(checks),'failures':[x for x in checks if not x['passed']]},indent=2))
raise SystemExit(0 if all(x['passed'] for x in checks) else 1)
