"""Check hand-specified CV arithmetic, source-to-mark mapping and actual label geometry.
Run render-selection-fixture.R first; this gate does not calculate expected values with package functions.
"""
from pathlib import Path
import argparse
import atexit
import json
import math
import os
import subprocess
import xml.etree.ElementTree as ET
from playwright.sync_api import sync_playwright
from selection_browser_helpers import assert_requested_tuple, run_adaptive_checks

parser = argparse.ArgumentParser()
parser.add_argument('--case-dir', type=Path, default=Path('/tmp/autoxplain-selection-cases'))
parser.add_argument('--output-dir', type=Path, required=True)
parser.add_argument('--browser', choices=['chromium', 'firefox', 'webkit'], default='chromium')
args = parser.parse_args()
args.output_dir.mkdir(parents=True, exist_ok=True)
checks = []
atexit.register(lambda: (args.output_dir/'selection-checks.json').write_text(json.dumps(checks, indent=2)))
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
expected_settings = {
    'linear_01': {}, 'tree_01': {'maxdepth': 2, 'cp': .03, 'minsplit': 8},
    'tree_02': {'maxdepth': 4, 'cp': .01, 'minsplit': 5},
    'neural_01': {'size': 1, 'decay': .1, 'maxit': 2000},
    'neural_02': {'size': 2, 'decay': .03, 'maxit': 2000}}
threshold = math.sqrt(7) + math.sqrt(20)/(2*math.sqrt(7))
source = json.loads((args.case_dir/'selection-source.json').read_text())
for candidate in source['candidates']:
    score = expected[candidate['configuration_id']][1]
    check('retained source score '+candidate['configuration_id'],
          candidate['cv_score'] is None if score is None else math.isclose(candidate['cv_score'], score))
with sync_playwright() as p:
    options = {'executable_path': os.environ['BROWSER_EXECUTABLE']} if os.environ.get('BROWSER_EXECUTABLE') else {}
    browser = getattr(p, args.browser).launch(**options)
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
              failed.locator(':scope > summary').evaluate('node=>node===document.activeElement') and
              'Fixture iteration limit' in failed.inner_text())
        check(f'failed configuration destination persists in URL {width}', page.url.endswith('#'+failure_id))
        page.reload(); settled(page)
        check(f'fresh deep link restores failed family and opens exact folds {width}',
              page.input_value('#selection-family-filter')=='neural' and failed.is_visible() and
              failed.evaluate('node=>node.open') and
              failed.locator(':scope > summary').evaluate('node=>node===document.activeElement'))
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
            geometry_script = '''svg=>{
              const b=svg.getBoundingClientRect();
              const texts=[...svg.querySelectorAll('text')].filter(t=>getComputedStyle(t).display!=='none'&&t.textContent.trim());
              // getBBox defaults to fill geometry, excluding the paper-colored
              // text stroke that Firefox includes in getBoundingClientRect.
              // Transform all corners so every check uses screen coordinates.
              const labels=texts.map(t=>{
                const box=t.getBBox(),matrix=t.getScreenCTM();
                if(!matrix)throw new Error('Visible SVG text has no screen transform');
                const corners=[[box.x,box.y],[box.x+box.width,box.y],
                  [box.x,box.y+box.height],[box.x+box.width,box.y+box.height]]
                  .map(([x,y])=>new DOMPoint(x,y).matrixTransform(matrix));
                const left=Math.min(...corners.map(p=>p.x)),right=Math.max(...corners.map(p=>p.x));
                const top=Math.min(...corners.map(p=>p.y)),bottom=Math.max(...corners.map(p=>p.y));
                const client=t.getBoundingClientRect();
                return {text:t.textContent,left,right,top,bottom,height:bottom-top,
                  client:{left:client.left,right:client.right,top:client.top,bottom:client.bottom}};
              });
              const clipped=labels.filter(a=>a.left<b.left-1||a.right>b.right+1||a.top<b.top-1||a.bottom>b.bottom+1).map(a=>a.text);
              const small=labels.filter(a=>a.height<12).map(a=>a.text);
              const overlaps=[];for(let i=0;i<labels.length;i++)for(let j=i+1;j<labels.length;j++){
                const a=labels[i],c=labels[j];
                if(a.left<c.right&&a.right>c.left&&a.top<c.bottom&&a.bottom>c.top)overlaps.push([a.text,c.text]);}
              return {labels,clipped,small,overlaps,
                ticks:[...svg.querySelectorAll('.selection-axis-tick')].filter(t=>getComputedStyle(t).display!=='none').map(t=>({value:+t.textContent,x:+t.getAttribute('x')})),
                threshold:+svg.querySelector('.selection-cutoff').getAttribute('x1'),
                rows:[...svg.querySelectorAll('.selection-plot-row')].map(r=>({
                  fold:[...r.querySelectorAll('.selection-fold-point')].map(x=>+x.getAttribute('cx')),
                  pooled:r.querySelector('.selection-score-point')?+r.querySelector('.selection-score-point').getAttribute('cx'):null}))};
            }'''
            geometry = figure.locator('svg').evaluate(geometry_script)
            check(f'label geometry {width} {family}', not geometry['clipped'] and not geometry['small'] and not geometry['overlaps'], geometry)
            if width == 390 and family == 'tree':
                row = figure.locator('.selection-plot-row').first
                role = row.locator('.selection-role')
                original_y = role.get_attribute('y')
                expected_pair = [row.locator('.selection-row-label').text_content(), role.text_content()]
                try:
                    role.evaluate('(node,y)=>node.setAttribute("y",y)', row.locator('.selection-row-label').get_attribute('y'))
                    settled(page)
                    overlap = figure.locator('svg').evaluate(geometry_script)
                    check('a real overlap between configuration and score text is rejected',
                          expected_pair in overlap['overlaps'], overlap)
                    figure.screenshot(path=str(args.output_dir/'text-overlap-negative-control.png'))
                finally:
                    role.evaluate('(node,y)=>node.setAttribute("y",y)', original_y)
                    settled(page)
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
                      detail.count() == 1 and detail.evaluate('node=>node.open')
                      and ident in detail.locator(':scope > summary').inner_text()
                      and detail.locator(':scope > summary').evaluate('node=>document.activeElement===node'))
                values = detail.locator('.selection-settings').inner_text()
                assert_requested_tuple(check, f'exact requested controls reach detail {width} {ident}',
                                       values, expected_settings[ident])
            precision.locator(':scope > summary').click(); settled(page)
            table_rows = figure.locator('.selection-candidate-table tbody tr')
            check(f'precision table can be opened explicitly {width} {family}',
                  figure.locator('.selection-candidate-table').is_visible() and table_rows.count()==len(candidate_rows))
            for index, candidate in enumerate(candidate_rows):
                row = table_rows.nth(index)
                ident = candidate['configuration_id']; score = expected[ident][1]
                actual = row.locator('td').nth(1).inner_text()
                assert_requested_tuple(check, f'precision view preserves exact requested tuple {width} {ident}',
                                       row.locator('td a').inner_text(), expected_settings[ident])
                check(f'precision view preserves pooled score {width} {ident}',
                      actual=='n/a' if score is None else abs(float(actual)-score)<.00006)
                link = row.locator('[data-selection-inspect]')
                target = link.get_attribute('data-selection-inspect')
                # A real pointer press spans frames. Scrolling between down and
                # up can send the click to the table body instead of this link.
                link.click(delay=60); settled(page)
                state = page.evaluate("""target=>({
                  visible:[...document.querySelectorAll('.selection-candidate:not([hidden])')]
                    .map(node=>({id:node.id,open:node.open})),
                  expanded:[...document.querySelectorAll('[data-selection-inspect]')]
                    .filter(node=>node.dataset.selectionInspect===target)
                    .map(node=>node.getAttribute('aria-expanded')),
                  focused:document.activeElement===document.getElementById(target)?.querySelector(':scope > summary')
                })""", target)
                check(f'precision link reaches same fold evidence as graph {width} {ident}',
                      state['visible']==[dict(id=target,open=True)] and state['focused']
                      and len(state['expanded'])==2 and all(value=='true' for value in state['expanded']),
                      dict(expected=target, **state))
            check(f'no page/table overflow {width} {family}', page.evaluate('()=>document.documentElement.scrollWidth<=innerWidth') and figure.locator('.selection-candidate-table').evaluate('x=>x.scrollWidth<=x.clientWidth+1'))
            precision.locator(':scope > summary').click(); settled(page)
            page.screenshot(path=str(args.output_dir/f'{family}-{width}.png'), full_page=True)
        page.close()
        page = browser.new_page(viewport={'width': width, 'height': 900})
        page.goto((args.case_dir/'selection-agreement.html').resolve().as_uri()); settled(page)
        agreed = page.locator('.selection-agreed')
        check(f'identical configuration uses one compact summary {width}', agreed.count()==1
              and page.locator('.selection-decision').count()==0)
        check(f'compact summary retains score, model, parameters and agreement {width}',
              all(text in agreed.inner_text() for text in ['2.6458 RMSE', 'Neural network', '1 unit; decay 0.1',
                  'Selected from 4 settings that completed cross-validation.']))
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
    precision.locator(':scope > summary').click()
    check('no-JS exact settings open natively',precision.locator('table').is_visible())
    page.close()
    page=browser.new_page(viewport={'width':1440,'height':900})
    page.goto((args.case_dir/'selection-oracle.html').resolve().as_uri());settled(page)
    page.select_option('#selection-family-filter','tree');settled(page)
    page.locator('.selection-family:not([hidden]) .selection-plot-link').nth(1).click();settled(page)
    page.locator('.selection-family:not([hidden]) .selection-rationale > summary').click();settled(page)
    def selection_state():
        return page.evaluate('''()=>({family:document.querySelector('#selection-family-filter').value,
          visible:[...document.querySelectorAll('.selection-family')].filter(x=>!x.hidden).map(x=>x.dataset.selectionFamily),
          opened:[...document.querySelectorAll('.selection-candidate')].filter(x=>!x.hidden&&x.open).map(x=>x.id)})''')
    before_print=selection_state()
    if args.browser == 'chromium':
        page.pdf(path=str(args.output_dir/'selection.pdf'),format='A4',print_background=True)
        settled(page)
        check('print preserves chosen family and opened candidate after returning',selection_state()==before_print,before_print)
        check('graph navigation is restored after printing', page.locator('.selection-plot-link').evaluate_all(
            'nodes=>nodes.every(node=>node.getAttribute("href")==="#"+node.dataset.selectionInspect)'))
    page.close()
    run_adaptive_checks(browser, args.case_dir, args.output_dir, check, settled, pdf_enabled=args.browser == 'chromium')
    browser.close()
# The PDF must contain the chosen tree setting's open evidence, not every family's
# hidden candidate details. Family names in the decision summary are still valid.
if args.browser == 'chromium':
    pdf_text=subprocess.check_output(['pdftotext',str(args.output_dir/'selection.pdf'),'-'],text=True)
    pdf_text=' '.join(pdf_text.split())
    check('PDF keeps the explicitly opened family rationale',
          'Preset tuples move from shallow trees and larger minimum splits toward deeper trees.' in pdf_text
          and 'Small single-layer networks and several weight penalties bound the search cost.' not in pdf_text
          and 'One unpenalized reference fit; there is no parameter grid for this family.' not in pdf_text)
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
print(json.dumps({'browser':args.browser,'checks':len(checks),'failures':[x for x in checks if not x['passed']]},indent=2))
raise SystemExit(0 if all(x['passed'] for x in checks) else 1)
