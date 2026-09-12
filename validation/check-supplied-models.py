"""Independent supplied-model disagreement, repeated-cost and decimal-cutoff browser acceptance.
Generate with render-supplied-models.R and render-cutoff-fixture.R. Requires Playwright.
"""
from pathlib import Path
import argparse
import json
import math
import os
import subprocess
import xml.etree.ElementTree as ET
from playwright.sync_api import sync_playwright
from report_payload import decode_data_payload, decode_prediction_payload

parser = argparse.ArgumentParser()
parser.add_argument('--case-dir', type=Path, default=Path('/tmp/autoxplain-explorer-cases'))
parser.add_argument('--output-dir', type=Path, required=True)
parser.add_argument('--axe-path', type=Path)
args = parser.parse_args(); args.output_dir.mkdir(parents=True, exist_ok=True)
checks = []
def check(name, passed, detail=None):
    checks.append(dict(check=name, passed=bool(passed), detail=detail))
def close(a, b, tol=1e-10):
    return a is not None and math.isclose(a, b, rel_tol=tol, abs_tol=tol)
def quantile(values, fraction):
    values = sorted(values); h = (len(values)-1)*fraction; i = math.floor(h)
    return values[i] + (values[min(i+1, len(values)-1)]-values[i])*(h-i)
def settled(page):
    page.evaluate('()=>new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)))')
def payload(page, ident):
    value = json.loads(page.locator('#'+ident).text_content())
    if ident == 'axr-data-payload':
        return decode_data_payload(value)
    if ident == 'axr-predictions-payload':
        return decode_prediction_payload(value)
    return value

bench = json.loads((args.case_dir/'supplied-benchmark-oracle.json').read_text())
source = json.loads((args.case_dir/'supplied-models-oracle.json').read_text())['binary']
for row in bench['summary']:
    records = [x for x in bench['measurements'] if x['model_id']==row['model_id'] and x['phase']=='measurement' and not x['error']]
    for i, record in enumerate(records):
        per_batch = record['elapsed_ms']/record['iterations']
        check(f'raw block/batch/row arithmetic {row["model_id"]} {i}',
              close(record['ms_per_batch'],per_batch) and close(record['ms_per_row'],per_batch/32) and record['batch_rows']==32)
    if row['median_ms_per_row'] is not None:
        independent = [x['elapsed_ms']/x['iterations'] for x in records]
        for key, fraction in [('p25_ms_per_batch',.25),('median_ms_per_batch',.5),('p75_ms_per_batch',.75)]:
            check(f'independent repeated quantile {row["model_id"]} {key}',close(row[key],quantile(independent,fraction)))
        check('independent repeated per-row median '+row['model_id'],close(row['median_ms_per_row'],quantile(independent,.5)/32))
    else:
        check('withheld benchmark explains its state '+row['model_id'],row['status']!='computed' and bool(row['reason']))

with sync_playwright() as p:
    options={'executable_path':os.environ['BROWSER_EXECUTABLE']} if os.environ.get('BROWSER_EXECUTABLE') else {}
    browser=p.chromium.launch(**options)
    for width in [1440,390]:
        page=browser.new_page(viewport={'width':width,'height':900})
        page.goto((args.case_dir/'supplied-binary-rows.html').resolve().as_uri()+'#evaluation');settled(page)
        detail=page.locator('.prediction-disagreement');detail.locator(':scope > summary').click();settled(page)
        text=detail.inner_text()
        check('reference excluded from disagreement '+str(width),'declared reference excluded' in text and 'Historical rate' not in text)
        gaps=[abs(a-b) for a,b in zip(source['predictions']['current'],source['predictions']['challenger'])]
        cells=detail.locator('table').first.locator('tbody tr').first.locator('td').all_text_contents()
        check('independent absolute pairwise distance '+str(width),close(float(cells[2]),sum(gaps)/len(gaps),.000051) and close(float(cells[3]),quantile(gaps,.9),.000051),cells)
        exported=payload(page,'axr-data-payload')['rows']
        retained={r['row_key']:r for r in exported if r['partition']=='evaluation' and r['retained']}
        links=detail.locator('.disagreement-records a')
        keys=links.evaluate_all('xs=>xs.map(x=>x.dataset.selectRow)')
        expected=sorted(retained,key=lambda key:-gaps[retained[key]['processed_position']-1])[:5]
        check('largest gaps restricted to exported records '+str(width),keys==expected,keys)
        if keys:
            check('disagreement link is inside handler workspace '+str(width),links.first.evaluate("x=>!!x.closest('.prediction-workspace')"))
            links.first.click();settled(page)
            state=page.evaluate('()=>window.AutoXplainRData.getState()')
            check('actual disagreement link selects exact source record '+str(width),state['selected']==keys[0] and state['view']=='records',state['selected'])
        page.close()
        page=browser.new_page(viewport={'width':width,'height':900})
        page.goto((args.case_dir/'supplied-binary-benchmark.html').resolve().as_uri());settled(page)
        text=page.locator('.prediction-benchmark').inner_text()
        # Closed details textContent retains complete aggregate table/protocol for inspection.
        text=page.locator('.prediction-benchmark').text_content()
        check('benchmark units and scope are explicit '+str(width),all(x in text for x in ['ms / row','ms / batch','not single-row request latency','not confidence intervals']))
        data=payload(page,'axr-data-payload');predictions=payload(page,'axr-predictions-payload')
        check('benchmark summary exports no individual records '+str(width),data.get('mode')=='summary' and not data.get('rows') and all(not x.get('cases') for x in predictions['models']))
        check('benchmark private sampling indices absent '+str(width),'sampling_rows' not in page.content())
        options=page.locator('#resource-select option').evaluate_all('xs=>xs.map(x=>x.value)')
        finite=[row for row in bench['summary'] if row['median_ms_per_row'] is not None]
        if finite:
            page.select_option('#resource-select','repeated_prediction_ms_per_row');settled(page)
            metric=page.input_value('#metric-select')
            chart=page.locator(f'[data-cost-plot="{metric}"][data-resource="repeated_prediction_ms_per_row"]')
            marks=chart.locator('[data-chart-source]').evaluate_all('xs=>xs.map(x=>({id:x.dataset.model,value:+x.dataset.x}))')
            expected_cost={row['model_id']:row['median_ms_per_row'] for row in finite}
            check('cost source uses finite repeated per-row medians '+str(width),len(marks)==len(finite) and all(close(x['value'],expected_cost[x['id']]) for x in marks),marks)
            check('visible repeated-cost axis has correct unit '+str(width),'Repeated prediction (ms / row)' in chart.inner_text())
        else:
            check('unavailable benchmark is not a selectable numeric cost '+str(width),'repeated_prediction_ms_per_row' not in options)
        table=page.locator('.benchmark-cost-table table')
        headers=table.locator('thead th').all_text_contents()
        rows=table.locator('tbody tr').evaluate_all('xs=>xs.map(x=>[...x.cells].map(c=>c.textContent))')
        check('compact five-column cost table '+str(width),len(headers)==5 and '25th–75th' in headers[3],headers)
        for i,row in enumerate(bench['summary']):
            if row['median_ms_per_row'] is None:
                check('withheld costs remain unavailable '+str(width)+row['model_id'],rows[i][1:4]==['Unavailable']*3)
            else:
                interval=[float(x.strip()) for x in rows[i][3].split('–')]
                check('displayed costs preserve significant digits '+str(width)+row['model_id'],
                      close(float(rows[i][1]),row['median_ms_per_row'],.0005) and
                      close(float(rows[i][2]),row['median_ms_per_batch'],.0005) and
                      all(close(x,y,.0005) for x,y in zip(interval,[row['p25_ms_per_batch'],row['p75_ms_per_batch']])) )
            check('repeat completion stays explicit '+str(width)+row['model_id'],rows[i][4]==f"{row['repetitions']} / {row['requested_repetitions']}")
        issues=[row for row in bench['summary'] if row['status']!='computed' or row['reason'] or row['warning']]
        issue_rows=page.locator('.benchmark-issues tbody tr').all_text_contents()
        check('only incomplete or annotated measurements have issue rows '+str(width),len(issue_rows)==len(issues))
        check('benchmark statuses reasons warnings retained '+str(width),all(row['status'] in issue_rows[i] and
              (not row['reason'] or row['reason'] in issue_rows[i]) and (not row['warning'] or row['warning'] in issue_rows[i])
              for i,row in enumerate(issues)))
        page.locator('.prediction-benchmark > summary').focus();page.keyboard.press('Enter');settled(page)
        check('benchmark details keyboard access '+str(width),page.locator('.prediction-benchmark').get_attribute('open') is not None)
        if args.axe_path:
            page.add_script_tag(path=str(args.axe_path))
            violations=page.evaluate("async()=> (await axe.run(document,{runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa','wcag22aa']}})).violations.map(x=>({id:x.id,nodes:x.nodes.map(n=>n.target)}))")
            check('benchmark accessibility '+str(width),not violations,violations)
        page.screenshot(path=str(args.output_dir/f'benchmark-{width}.png'),full_page=True)
        if width==1440:
            page.evaluate("""()=>{
              window.AutoXplainRReport.selectModel('challenger');
              window.printScreenState=window.AutoXplainRReport.getState();
              window.printScreenLinks=[...document.querySelectorAll('a[href^="#"]')]
                .map(link=>({link,href:link.getAttribute('href')}));
              window.printExternalLinks=[...document.querySelectorAll('a[href^="https:"]')]
                .map(link=>({link,href:link.getAttribute('href')}));
              // Registered after production handlers: simulate a print redraw
              // introducing an SVG link after the shared print handler ran.
              addEventListener('beforeprint',()=>{
                const probe=document.createElement('div');probe.id='print-link-probe';
                probe.innerHTML='<svg width="300" height="25"><a href="#absent-print-target">'+
                  '<text x="0" y="18">Printed model shortcut</text></a></svg>'+
                  '<p><a href="https://doi.org/10.1214/aos/1176344136">External statistical reference</a></p>';
                document.body.append(probe);
                queueMicrotask(()=>{window.printLinkObserved={
                  fragments:document.querySelectorAll('a[href^="#"]').length,
                  lateText:probe.querySelector('svg').textContent,
                  citation:probe.querySelector('p a').getAttribute('href'),
                  state:window.AutoXplainRReport.getState()
                };});
              },{once:true});
            }""")
            page.pdf(path=str(args.output_dir/'benchmark.pdf'),format='A4',print_background=True)
            settled(page)
            printed=page.evaluate('window.printLinkObserved')
            check('print removes fragment destinations including later SVG redraws',
                  printed['fragments']==0 and printed['lateText']=='Printed model shortcut',printed)
            check('print preserves selected model and views',page.evaluate('''()=>
              JSON.stringify(window.printScreenState)===JSON.stringify(window.printLinkObserved.state) &&
              JSON.stringify(window.printScreenState)===JSON.stringify(window.AutoXplainRReport.getState())'''))
            check('print restores original fragment links and later SVG links',page.evaluate('''()=>
              window.printScreenLinks.length>0 && window.printScreenLinks.every(({link,href})=>link.getAttribute('href')===href) &&
              document.querySelector('#print-link-probe svg a').getAttribute('href')==='#absent-print-target' '''))
            check('external citations keep their destinations during and after print',page.evaluate('''()=>
              window.printLinkObserved.citation==='https://doi.org/10.1214/aos/1176344136' &&
              window.printExternalLinks.every(({link,href})=>link.getAttribute('href')===href)'''))
            page.locator('#print-link-probe').evaluate('node=>node.remove()')
        page.close()
    for mode in ['none','summary']:
        page=browser.new_page();page.goto((args.case_dir/f'supplied-binary-{mode}.html').resolve().as_uri());settled(page)
        check('disagreement has no source links in '+mode,page.locator('.disagreement-records a').count()==0)
        page.close()
    page=browser.new_page();page.goto((args.case_dir/'cutoff-decimal.html').resolve().as_uri());settled(page)
    slider=page.locator('[data-prediction-cutoff]')
    for cutoff in range(101):
        slider.fill(str(cutoff));slider.dispatch_event('input')
        counts=page.locator('[data-confusion-table] td').evaluate_all('xs=>xs.map(x=>({observed:x.dataset.observed,predicted:x.dataset.predicted,count:+x.querySelector("[data-cell-count]").textContent}))')
        positive=cutoff<=57
        check('literal0.57 probability at displayed cutoff '+str(cutoff),all(x['count']==int((x['predicted']=='yes')==positive) for x in counts),counts if cutoff==57 else None)
    slider.fill('56');slider.dispatch_event('input');slider.focus();page.keyboard.press('ArrowRight')
    check('keyboard reaches exact0.57 boundary',slider.input_value()=='57' and page.locator('[data-cutoff-metric="fp"]').text_content()=='1')
    page.close();browser.close()
pdf_result=subprocess.run(['pdftohtml','-xml','-stdout','-zoom','1',str(args.output_dir/'benchmark.pdf')],
                          text=True,capture_output=True,check=True)
check('printed report has no broken named destinations','Bad named destination' not in pdf_result.stderr,pdf_result.stderr)
root=ET.fromstring(pdf_result.stdout)
check('printed PDF preserves a usable external citation',
      any(node.attrib.get('href')=='https://doi.org/10.1214/aos/1176344136' for node in root.iter('a')))
fonts={node.attrib['id']:float(node.attrib['size']) for node in root.iter('fontspec')}
printed=[(''.join(node.itertext()),fonts[node.attrib['font']]) for node in root.iter('text')
         if 'Repeated prediction' in ''.join(node.itertext()) or 'Median ms' in ''.join(node.itertext())]
check('actual benchmark print cost labels at least8pt',len(printed)>0 and all(size>=8 for _,size in printed),printed)
(args.output_dir/'report-costs-checks.json').write_text(json.dumps(checks,indent=2))
print(json.dumps({'checks':len(checks),'failures':[x for x in checks if not x['passed']]},indent=2))
raise SystemExit(0 if all(x['passed'] for x in checks) else 1)
