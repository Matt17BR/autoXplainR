"""Run after render-cutoff-cases.R. Expected scores/classes are literal source values, not report payload answers."""
from playwright.sync_api import sync_playwright
from report_payload import decode_prediction_payload
from pathlib import Path
import json,math,argparse,os,hashlib,platform,importlib.metadata
parser=argparse.ArgumentParser(description="Check literal cutoff case decisions, row links and privacy against independent hand values.")
parser.add_argument('--case-dir',type=Path,default=Path('/tmp/autoxplain-explorer-cases'))
parser.add_argument('--output-dir',type=Path,required=True)
args=parser.parse_args()
base=args.case_dir.resolve();args.output_dir.mkdir(parents=True,exist_ok=True)
records=[]
def check(name,passed,evidence=None):records.append({'name':name,'pass':bool(passed),'evidence':evidence})
scores=[0,.01,.49,.5,.57,.58,.99,1]*2
truth=['yes','no','yes','no']*4
with sync_playwright() as p:
 b=p.chromium.launch(**({'executable_path':os.environ['BROWSER_EXECUTABLE']} if os.environ.get('BROWSER_EXECUTABLE') else {}))
 browser_version=b.version
 page=b.new_page(viewport={'width':1440,'height':1000});errors=[];page.on('pageerror',lambda e:errors.append(str(e)))
 page.goto((base/'cutoff-cases-rows.html').as_uri()+'#evaluation')
 payload=decode_prediction_payload(json.loads(page.locator('#axr-predictions-payload').text_content()))
 page.evaluate("window.__selected=[];addEventListener('axr:row-selected',e=>__selected.push(e.detail.row_key))")
 for model in payload['models']:
  id=model['model_id'];page.evaluate('(id)=>AutoXplainRReport.selectModel(id)',id)
  panel=page.locator('[data-prediction-model]:visible');panel.locator('.prediction-records summary').click()
  source_keys={row['source_row']:row['row_key'] for row in model['cases']}
  source_positions={row['source_row']:row['processed_position'] for row in model['cases']}
  check(id+': source row positions retained',source_positions=={i:i for i in range(1,17)})
  for cut in (0,.5,.57,1):
   panel.locator('[data-prediction-cutoff]').fill(str(round(cut*100)))
   expected=[]
   for i,(score,observed) in enumerate(zip(scores,truth)):
    prob=1-score if id=='inverse' else score
    predicted='yes' if prob>=cut else 'no'
    expected.append(dict(key=source_keys[i+1],source=i+1,observed=observed,predicted=predicted,pobs=prob if observed=='yes' else 1-prob,ppred=prob if predicted=='yes' else 1-prob,wrong=observed!=predicted,index=i))
   expected=sorted(expected,key=lambda r:(not r['wrong'],r['pobs'],r['index']))[:10]
   actual=panel.locator('.prediction-cases tbody tr').evaluate_all('xs=>xs.map(r=>({key:r.dataset.caseRow,cells:[...r.cells].map(c=>c.textContent)}))')
   match=len(actual)==len(expected)
   for actual_row,e in zip(actual,expected):
    cells=actual_row['cells'];match=match and actual_row['key']==e['key'] and cells[1:3]==[e['observed'],e['predicted']] and math.isclose(float(cells[3]),e['pobs'],abs_tol=1e-8) and math.isclose(float(cells[4]),e['ppred'],abs_tol=1e-8)
   check(f'{id}: literal source answers and wrong-first order at {cut}',match,actual if not match else None)
   check(f'{id}: caption at {cut}',f'cutoff {cut:.2f}' in panel.locator('.prediction-cases caption').inner_text())
   code=panel.locator('[data-prediction-code]').text_content();check(f'{id}: R decision rule at {cut}',f'>= {cut:.2f}' in code and model['r_code']['prediction'] in code,code)
  panel.locator('[data-prediction-cutoff]').fill('57')
  selected=panel.locator('.prediction-cases [data-select-row]').first.get_attribute('data-select-row')
  panel.locator('.prediction-cases [data-select-row]').first.click()
  check(id+': dynamic source link dispatches selected row',page.evaluate('__selected.at(-1)')==selected)
  check(id+': dynamic source link opens data workspace',page.evaluate('AutoXplainRReport.getState().page')=='data')
  page.locator('[data-page-link=evaluation]').click()
  panel.locator('[data-prediction-cutoff]').fill('100');panel.locator('[data-prediction-cutoff]').fill('57')
  check(id+': selected row survives cutoff reordering',panel.locator('.prediction-cases .is-selected').count()==1 and panel.locator('.prediction-cases .is-selected').get_attribute('data-case-row')==selected, {'selected':selected,'events':page.evaluate('__selected'),'rows':panel.locator('.prediction-cases tbody tr').evaluate_all('xs=>xs.map(r=>[r.dataset.caseRow,r.className])')})
  panel.locator('.prediction-records summary').click()
 check('zero runtime errors',not errors,errors)
 for mode in ('summary','none'):
  page.goto((base/f'cutoff-cases-{mode}.html').as_uri()+'#evaluation')
  for val in ('0','57','100'):page.locator('[data-prediction-model]:visible [data-prediction-cutoff]').fill(val)
  check(mode+': no individual case nodes created',page.locator('[data-case-row]').count()==0)
 b.close()
summary={'checks':records,'runtime':{'python':platform.python_version(),'playwright':importlib.metadata.version('playwright'),'browser':browser_version},
         'fixture_sha256':{mode:hashlib.sha256((base/f'cutoff-cases-{mode}.html').read_bytes()).hexdigest() for mode in ('rows','summary','none')}}
(args.output_dir/'cutoff-cases-checks.json').write_text(json.dumps(summary,indent=2));print(json.dumps({'checks':len(records),'failures':[r for r in records if not r['pass']]}))
assert all(r['pass'] for r in records)
