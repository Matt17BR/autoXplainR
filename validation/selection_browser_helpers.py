"""Independent browser oracles for declared synthetic adaptive selection records."""
import math
import re
import subprocess
import xml.etree.ElementTree as ET
from playwright.sync_api import TimeoutError as PlaywrightTimeoutError


def assert_requested_tuple(check, name, text, expected):
    """Require every named value to round-trip exactly, including adjacent doubles."""
    shown = text.removeprefix('Search settings:').strip()
    if not expected:
        check(name, shown == 'No tuned controls', {'expected': expected, 'displayed': text})
        return
    try:
        pairs = [part.split(' = ', 1) for part in shown.split('; ')]
        actual = dict(pairs)
        valid = len(actual) == len(pairs) and set(actual) == set(expected)
        valid = valid and all(
            float(actual[key]) == value if isinstance(value, (float, int))
            else actual[key] == value for key, value in expected.items())
    except (ValueError, KeyError, TypeError):
        valid = False
    check(name, valid, {'expected': expected, 'displayed': text})


# These constants are deliberately separate from the R fixture and rendered
# metadata. The checker never calls package scoring/ranking/formatting code.
BOOSTING_SETTINGS = {
    f'boosting_0{i}': dict(nrounds=4096 if i == 7 else 2000, eta=.05, max_depth=6,
        min_child_weight=1, subsample=.8, colsample_bytree=.8,
        reg_alpha=.1 if i == 2 else .10000000000000002 if i == 7 else 0,
        reg_lambda=10 if i == 2 else 1.0000000000000002 if i == 7 else 1, encoding='matrix')
    for i in range(1, 8)
}
SCREEN_ORDERS = {
    'log_loss': ['boosting_02', 'boosting_07', 'boosting_05', 'boosting_01',
                 'boosting_03', 'boosting_04', 'boosting_06'],
    'roc_auc': ['boosting_01', 'boosting_05', 'boosting_02', 'boosting_07',
                'boosting_03', 'boosting_04', 'boosting_06']}
SCREEN_SCORES = {'boosting_01': .8, 'boosting_02': .6, 'boosting_05': .7, 'boosting_07': .6}
SCREEN_STATUSES = {'boosting_01': 'Selected for CV', 'boosting_02': 'Not advanced',
    'boosting_03': 'Failed', 'boosting_04': 'Not attempted', 'boosting_05': 'Selected for CV',
    'boosting_06': 'No valid score', 'boosting_07': 'Not advanced'}
MONITORING = {
    'log_loss': {'cv': [([.8, .55, .3, .35, .4], 3), ([.9, .7, .5, .4, .45, .5], 4)],
                 'screen': [([.9, .6, .4, .5], 3)]},
    'roc_auc': {'cv': [([.2, .45, .7, .65, .6], 3), ([.1, .3, .5, .6, .55, .5], 4)],
                'screen': [([.1, .4, .6, .5], 3)]}}


def target(ident):
    return 'selection-detail-' + ident.encode().hex()


def check_round_chart(chart, expected, name, check):
    geometry = chart.locator('svg').evaluate('''svg=>{
      const box=svg.getBoundingClientRect();
      const texts=[...svg.querySelectorAll('text')].filter(t=>getComputedStyle(t).display!=='none'&&t.textContent.trim());
      const labels=texts.map(t=>{const b=t.getBoundingClientRect(),m=t.getScreenCTM();return {
        text:t.textContent,left:b.left,right:b.right,top:b.top,bottom:b.bottom,
        font:parseFloat(getComputedStyle(t).fontSize)*Math.hypot(m.a,m.b)}});
      const overlaps=[];for(let i=0;i<labels.length;i++)for(let j=i+1;j<labels.length;j++){
        const a=labels[i],b=labels[j];if(a.left<b.right-.1&&a.right>b.left+.1&&a.top<b.bottom-.1&&a.bottom>b.top+.1)
          overlaps.push([a.text,b.text]);}
      return {labels,overlaps,clipped:labels.filter(t=>t.left<box.left-1||t.right>box.right+1||t.top<box.top-1||t.bottom>box.bottom+1),
        x:[...svg.querySelectorAll('.selection-round-x-tick')].map(t=>({value:+t.dataset.value,pos:+t.getAttribute('x')})),
        y:[...svg.querySelectorAll('.selection-round-grid')].map(t=>({value:+t.dataset.value,pos:+t.getAttribute('y1')})),
        paths:[...svg.querySelectorAll('.selection-round-curve')].map(p=>({d:p.getAttribute('d'),
          rounds:p.dataset.rounds.split(',').map(Number),scores:p.dataset.scores.split(',').map(Number)})),
        selected:[...svg.querySelectorAll('[data-selected-round]')].map(t=>({round:+t.dataset.selectedRound,
          score:+t.dataset.selectedScore,x:+t.getAttribute('cx'),y:+t.getAttribute('cy')}))};
    }''')
    check(name + ' physical labels are at least 12px and fit without overlaps',
          len(geometry['labels']) >= 4 and all(t['font'] >= 11.99 for t in geometry['labels'])
          and not geometry['clipped'] and not geometry['overlaps'], geometry)
    ticks_valid = len(geometry['x']) >= 2 and len(geometry['y']) >= 2
    check(name + ' has independently usable numeric axes', ticks_valid, geometry)
    if not ticks_valid:
        return
    def position(value, axis):
        first, second = geometry[axis][:2]
        return first['pos'] + (value-first['value']) * (second['pos']-first['pos']) / (second['value']-first['value'])
    check(name + ' contains exactly the declared monitoring curves and selections',
          len(geometry['paths']) == len(expected) and len(geometry['selected']) == len(expected))
    for index, (scores, selected) in enumerate(expected):
        path = geometry['paths'][index]
        marker = geometry['selected'][index]
        # The fixture uses 1-loss for AUC, whose binary rounding may differ by
        # one ulp from a decimal literal. These are scores, not exact settings.
        check(name + f' trace {index+1} keeps every actually tried round and score',
              path['rounds'] == list(range(1, len(scores)+1)) and len(path['scores']) == len(scores)
              and all(math.isclose(a, b, abs_tol=1e-14) for a, b in zip(path['scores'], scores)))
        points = re.findall(r'[-+]?(?:\d*\.\d+|\d+)(?:[eE][-+]?\d+)?', path['d'])
        points = list(zip(map(float, points[::2]), map(float, points[1::2])))
        check(name + f' trace {index+1} is plotted on the declared numeric axes',
              len(points) == len(scores) and all(abs(x-position(i+1, 'x')) < .02
              and abs(y-position(score, 'y')) < .02 for i, ((x, y), score) in enumerate(zip(points, scores))))
        check(name + f' independently selected round {index+1} lands on its recorded point',
              marker['round'] == selected and math.isclose(marker['score'], scores[selected-1], abs_tol=1e-14)
              and abs(marker['x']-position(selected, 'x')) < .001
              and abs(marker['y']-position(scores[selected-1], 'y')) < .001, marker)


def run_adaptive_checks(browser, case_dir, output_dir, check, settled, pdf_enabled=True):
    for metric, expected_order in SCREEN_ORDERS.items():
        uri = (case_dir / f'selection-adaptive-{metric}.html').resolve().as_uri()
        for width in [1440, 390, 320]:
            name = f'adaptive {metric} {width}'
            page = browser.new_page(viewport={'width': width, 'height': 900})
            page.goto(uri); settled(page)
            check(name + ' labels the hand-specified fixture',
                  'Synthetic acceptance fixture' in page.locator('body > p').inner_text())
            check(name + ' opens the declared regularized primary family',
                  page.input_value('#selection-family-filter') == 'regularized')
            regularized = page.locator('.selection-family[data-selection-family="regularized"]')
            precision = regularized.locator('.selection-precision')
            precision.locator(':scope > summary').click(); settled(page)
            assert_requested_tuple(check, name + ' regularized penalties round-trip in precision table',
                precision.locator('tbody tr td a').inner_text(), {'alpha': .5, 'lambda_fraction': .01})
            # Start in another family; the screen anchor must choose its own
            # family, open just its target, move focus, and survive reload.
            page.select_option('#selection-family-filter', 'all'); settled(page)
            family = page.locator('.selection-family[data-selection-family="boosting"]')
            link = family.locator(f'.selection-screening-table a[href="#{target("boosting_07")}"]')
            link.focus(); page.keyboard.press('Enter'); settled(page)
            detail = page.locator('#' + target('boosting_07'))
            navigation = dict(family=page.input_value('#selection-family-filter'), visible=detail.is_visible(),
                opened=detail.evaluate('n=>n.open'), details=page.locator('.selection-candidate:not([hidden])').count(),
                focused=detail.locator(':scope > summary').evaluate('n=>n===document.activeElement'), url=page.url)
            check(name + ' keyboard screening link selects exact detail and family',
                  page.input_value('#selection-family-filter') == 'boosting' and detail.is_visible()
                  and detail.evaluate('n=>n.open') and page.locator('.selection-candidate:not([hidden])').count() == 1
                  and detail.locator(':scope > summary').evaluate('n=>n===document.activeElement')
                  and page.url.endswith('#' + target('boosting_07')), navigation)
            page.reload(); settled(page)
            check(name + ' screening destination survives reload with focus',
                  page.input_value('#selection-family-filter') == 'boosting' and detail.is_visible()
                  and detail.evaluate('n=>n.open') and detail.locator(':scope > summary').evaluate('n=>n===document.activeElement'))
            rows = family.locator('.selection-screening-table tbody tr')
            records = rows.evaluate_all('rows=>rows.map(r=>({id:r.querySelector("small").textContent,'
                'score:r.cells[1].textContent,status:r.cells[2].textContent}))')
            check(name + ' ranks successful screening scores with deterministic ID ties',
                  [r['id'] for r in records] == expected_order, records)
            check(name + ' labels the screening direction separately from CV',
                  family.locator('.selection-screening-table caption').inner_text() ==
                  f'Common training-only sample; {"higher" if metric == "roc_auc" else "lower"} is better. Compare CV separately below.')
            check(name + ' shows screening before CV chart', family.evaluate('f=>'
                'f.querySelector(".selection-screening").compareDocumentPosition(f.querySelector(".selection-chart-wrap")) & Node.DOCUMENT_POSITION_FOLLOWING'))
            for record in records:
                ident = record['id']
                check(name + ' preserves screen status and score ' + ident,
                      record['status'] == SCREEN_STATUSES[ident] and
                      (float(record['score']) == SCREEN_SCORES[ident] if ident in SCREEN_SCORES else record['score'] == 'Not ranked'), record)
                link = family.locator(f'.selection-screening-table a[href="#{target(ident)}"]')
                link.focus(); page.keyboard.press('Enter'); settled(page)
                detail = page.locator('#' + target(ident))
                check(name + ' screening link targets only ' + ident,
                      detail.is_visible() and detail.evaluate('n=>n.open')
                      and page.locator('.selection-candidate:not([hidden])').count() == 1)
                assert_requested_tuple(check, name + ' full requested tuple in detail ' + ident,
                    detail.locator('.selection-settings').inner_text(), BOOSTING_SETTINGS[ident])
                if ident == 'boosting_05':
                    check(name + ' screening promotion survives later failed CV',
                          'Synthetic CV failure after successful screening promotion' in detail.inner_text()
                          and record['status'] == 'Selected for CV')
                if ident == 'boosting_04':
                    check(name + ' unattempted screening has explicit evidence',
                          'No screening fit was started for this configuration.' in detail.inner_text())
            precision = family.locator('.selection-precision')
            precision.locator(':scope > summary').click(); settled(page)
            for ident, wanted in BOOSTING_SETTINGS.items():
                link = precision.locator(f'a[href="#{target(ident)}"]')
                assert_requested_tuple(check, name + ' full requested tuple in precision table ' + ident,
                                       link.inner_text(), wanted)
            family.locator(f'.selection-screening-table a[href="#{target("boosting_01")}"]').click(); settled(page)
            detail = page.locator('#' + target('boosting_01'))
            check(name + ' effective CV and final rounds remain distinct from requested cap',
                  'Full-training fit: 4 rounds.' in detail.inner_text()
                  and 'rounded-up median of 2 fold choices (3, 4)' in detail.inner_text())
            fold_rows = detail.locator('table').filter(has=page.locator('caption', has_text='boosting_01 fold scores and stopping choices')).evaluate('''table=>{
              const headers=[...table.querySelectorAll('thead th')].map(n=>n.textContent);
              return [...table.querySelectorAll('tbody tr')].map(row=>Object.fromEntries([...row.cells].map((cell,i)=>[headers[i],cell.textContent])));
            }''')
            metric_column = 'CV score' if metric == 'roc_auc' else 'CV loss'
            wanted_scores = [.80, .90] if metric == 'roc_auc' else [.35, .45]
            check(name + ' fold table retains independent scores, rows, chosen and tried rounds', len(fold_rows) == 2
                and all(float(row[metric_column]) == wanted_scores[index] and int(row['Fold']) == index+1
                    and int(row['Chosen rounds']) == [3, 4][index] and int(row['Tried rounds']) == [5, 6][index]
                    and int(row['Training rows']) == 80 and int(row['Validation rows']) == 80
                    for index, row in enumerate(fold_rows)), fold_rows)
            check_round_chart(detail.locator(':scope > .selection-round-chart'), MONITORING[metric]['cv'], name + ' CV stopping', check)
            screen = detail.locator(':scope > details').filter(has=page.locator('summary', has_text='Screening settings and stopping trace'))
            screen.locator(':scope > summary').click(); settled(page)
            check_round_chart(screen.locator('.selection-round-chart'), MONITORING[metric]['screen'], name + ' screening stopping', check)
            technical = screen.locator('table').inner_text()
            check(name + ' screening retains its separate 128-round cap and effective three rounds',
                  re.search(r'Requested / Nrounds\s+128(?:\s|$)', technical) is not None
                  and re.search(r'Effective / Nrounds\s+3(?:\s|$)', technical) is not None)
            overflow = page.evaluate('''()=>({page:document.documentElement.scrollWidth,viewport:innerWidth,
              tables:[...document.querySelectorAll('.table-wrap')].filter(n=>{
                for(let p=n.parentElement;p;p=p.parentElement)if(p.matches('details:not([open])')&&!p.firstElementChild.contains(n))return false;
                return n.getBoundingClientRect().width>0;
              }).map(n=>{const b=n.getBoundingClientRect();return {caption:n.querySelector('caption')?.textContent,
                width:n.clientWidth,scroll:n.scrollWidth,left:b.left,right:b.right,overflow:getComputedStyle(n).overflowX,
                exact:n.classList.contains('selection-candidate-table')||n.classList.contains('selection-family-overview'),focusable:n.tabIndex>=0,
                labelled:n.getAttribute('role')==='region'&&!!(n.getAttribute('aria-label')||n.getAttribute('aria-labelledby'))}})})''')
            check(name + ' page and table wrappers fit viewport',
                  overflow['page'] <= overflow['viewport'] and all(t['left'] >= -1
                  and t['right'] <= overflow['viewport']+1 for t in overflow['tables']), overflow)
            check(name + ' family summary, screening and exact settings need no horizontal scrolling',
                  all(t['scroll'] <= t['width']+1 for t in overflow['tables'] if t['exact']), overflow)
            check(name + ' wide evidence tables provide labelled keyboard scroll regions',
                  all(t['overflow'] in ('auto', 'scroll') and t['focusable'] and t['labelled']
                      for t in overflow['tables'] if t['scroll'] > t['width']+1), overflow)
            fold_wrapper = detail.locator('.table-wrap').filter(has=page.locator('caption', has_text='boosting_01 fold scores and stopping choices'))
            if fold_wrapper.evaluate('n=>n.scrollWidth>n.clientWidth+1'):
                fold_wrapper.evaluate('n=>{n.scrollLeft=0}')
                fold_wrapper.focus(); page.keyboard.press('ArrowRight')
                try:
                    page.wait_for_function('n=>n.scrollLeft>0', arg=fold_wrapper.element_handle(), timeout=1500)
                    scrolls = True
                except PlaywrightTimeoutError:
                    scrolls = False
                check(name + ' keyboard reaches horizontally scrollable fold columns', scrolls)
            page.screenshot(path=str(output_dir / f'adaptive-{metric}-{width}.png'), full_page=True)
            if width == 1440 and pdf_enabled:
                before = page.url
                pdf = output_dir / f'adaptive-{metric}.pdf'
                page.pdf(path=str(pdf), format='A4', print_background=True)
                settled(page)
                check(name + ' printing preserves chosen screening detail', page.url == before
                      and detail.evaluate('n=>n.open') and page.input_value('#selection-family-filter') == 'boosting')
                text = ' '.join(subprocess.check_output(['pdftotext', str(pdf), '-'], text=True).split())
                check(name + ' PDF retains distinct CV and screening monitoring evidence',
                      'boosting_01 fold scores and stopping choices' in text and
                      'boosting_01 screening result' in text and 'Screening technical record' in text
                      and 'Fold 1 · round 3' in text and 'Fold 2 · round 4' in text and 'Screening · round 3' in text)
                xml = subprocess.run(['pdftohtml', '-xml', '-stdout', '-zoom', '1', str(pdf)],
                                     text=True, capture_output=True, check=True)
                root = ET.fromstring(xml.stdout)
                fonts = {node.attrib['id']: float(node.attrib['size']) for node in root.iter('fontspec')}
                labels = [(''.join(node.itertext()), fonts[node.attrib['font']]) for node in root.iter('text')
                          if 'Inner ' in ''.join(node.itertext()) and 'is better' in ''.join(node.itertext())
                          or ''.join(node.itertext()) == 'Boosting round']
                check(name + ' PDF monitoring chart labels remain at least 8pt',
                      len(labels) >= 4 and all(size >= 8 for _, size in labels), labels)
            page.close()
        page = browser.new_page(viewport={'width': 320, 'height': 900}, java_script_enabled=False)
        page.goto(uri)
        check(f'adaptive {metric} no-JS retains all screening and candidate records',
              page.locator('.selection-screening-table tbody tr').count() == 8
              and page.locator('.selection-candidate').count() == 8)
        detail = page.locator('#' + target('boosting_07'))
        detail.locator(':scope > summary').click()
        assert_requested_tuple(check, f'adaptive {metric} no-JS adjacent penalties remain exact',
                               detail.locator('.selection-settings').inner_text(), BOOSTING_SETTINGS['boosting_07'])
        check(f'adaptive {metric} no-JS open detail fits mobile',
              detail.is_visible() and page.evaluate('()=>document.documentElement.scrollWidth<=innerWidth'))
        page.close()
