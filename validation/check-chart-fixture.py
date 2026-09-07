from pathlib import Path
import json, math, argparse, os
from report_geometry import effect_geometry
from playwright.sync_api import sync_playwright
parser=argparse.ArgumentParser(description="Check independent hand-chart oracles, label geometry and no-JavaScript readability.")
parser.add_argument('--case-dir',type=Path,default=Path('/tmp/autoxplain-explorer-cases'))
parser.add_argument('--output-dir',type=Path,required=True)
args=parser.parse_args()
base=args.case_dir; args.output_dir.mkdir(parents=True,exist_ok=True)
checks=[]
def check(label,ok,detail=None):
 checks.append(dict(check=label,pass_=bool(ok),detail=detail))
def settled(page):page.evaluate('()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve)))')
with sync_playwright() as p:
 browser=p.chromium.launch(**({'executable_path':os.environ['BROWSER_EXECUTABLE']} if os.environ.get('BROWSER_EXECUTABLE') else {}))
 for width in [1440,390,320]:
  page=browser.new_page(viewport={'width':width,'height':844});page.goto((base/'chart-oracle.html').as_uri());settled(page)
  result=page.locator('[data-kind=cost] svg').evaluate('''svg=>{const texts=[...svg.querySelectorAll('text')].map(t=>({v:Number(t.textContent),text:t.textContent,x:Number(t.getAttribute('x')),y:Number(t.getAttribute('y')),b:t.getBoundingClientRect().toJSON()}));const axis=svg.querySelector('.axr-axis'),left=+axis.getAttribute('x1'),bottom=+axis.getAttribute('y1');return {xt:texts.filter(t=>Number.isFinite(t.v)&&t.y===bottom+20),yt:texts.filter(t=>Number.isFinite(t.v)&&t.x===left-8),points:[...svg.querySelectorAll('[data-chart-point]')].map(g=>({id:g.dataset.modelId,cx:+g.querySelector('circle').getAttribute('cx'),cy:+g.querySelector('circle').getAttribute('cy')})),labels:texts.filter(t=>!Number.isFinite(t.v)&&t.text.match(/regression|network|tree|baseline/))}}''')
  expected={'linear':(116.1,2.997),'neural':(106.4,3.007),'tree':(85.08,3.952),'baseline':(79.7,7.565)}
  def interpolate(ticks,value,axis):
   a,b=ticks[:2];return a[axis]+(value-a['v'])/(b['v']-a['v'])*(b[axis]-a[axis])
  for point in result['points']:
   x,y=expected[point['id']];dx=abs(point['cx']-interpolate(result['xt'],x,'x'));dy=abs(point['cy']-(interpolate(result['yt'],y,'y')-4))
   check(f'cost independent x/y geometry {width} {point["id"]}',max(dx,dy)<.01,dict(dx=dx,dy=dy))
  for i,a in enumerate(result['labels']):
   for b in result['labels'][i+1:]:
    overlap=a['b']['left']<b['b']['right'] and a['b']['right']>b['b']['left'] and a['b']['top']<b['b']['bottom'] and a['b']['bottom']>b['b']['top']
    check(f'direct labels do not overlap {width} {a["text"]}/{b["text"]}',not overlap)
  cost_figure=page.locator('[data-kind=cost]')
  check(f'chart methodology starts collapsed {width}',not cost_figure.locator('.axr-chart-guidance').evaluate('el=>el.open'))
  check(f'chart detail is quiet before inspection {width}',not cost_figure.locator('.axr-chart-detail').is_visible())
  cost_figure.locator('[data-chart-point]').first.focus()
  check(f'keyboard inspection exposes exact point detail {width}',cost_figure.locator('.axr-chart-detail').is_visible()
        and 'Linear regression' in cost_figure.locator('.axr-chart-detail').inner_text())
  page.locator('[data-page-link=patterns]').click();settled(page)
  page.select_option('#comparison-model-select','Neural network');settled(page)
  figure=page.locator('[data-kind=effect]')
  check(f'comparison visible {width}',set((figure.get_attribute('data-visible-models')or'').split(','))=={'Linear regression','Neural network'})
  support=figure.locator('.axr-support').evaluate_all('xs=>xs.map(x=>({w:+x.getAttribute("width"),h:+x.getAttribute("height")}))')
  # Actual support widths match intervals 2,8,20,70,300, including the narrowest bin.
  check(f'unequal ALE interval width {width}',math.isclose(support[-1]['w']/support[-2]['w'],300/70,rel_tol=1e-8))
  check(f'ALE count heights {width}',math.isclose(support[-1]['h']/support[-2]['h'],2/15,rel_tol=1e-8))
  matched,evidence=effect_geometry(page.locator('#patterns'),dict(distance_km=[0,2,10,30,100,400],accumulated_effect=[-.0000001,-.00000008,-.00000003,.00000003,.00000007,.00000015],n=[None,2,8,40,15,2]),'Linear regression')
  check(f'tiny signed effects match independently known values {width}',matched,evidence)
  table=figure.locator('table tbody tr').evaluate_all('xs=>xs.map(x=>[...x.cells].map(c=>c.textContent))')
  check(f'table preserves tiny nonzero effect {width}',any(row[0]=='Linear regression' and math.isclose(float(row[2]),-1e-7,abs_tol=1e-12) for row in table))
  clipped=page.locator('.axr-chart svg').evaluate_all('''svgs=>svgs.flatMap(svg=>[...svg.querySelectorAll('text')].filter(t=>{let a=t.getBoundingClientRect(),b=svg.getBoundingClientRect();return a.left<b.left-1||a.right>b.right+1||a.top<b.top-1||a.bottom>b.bottom+1}).map(t=>t.textContent))''')
  check(f'long categorical labels fit {width}',not clipped,clipped)
  page.close()
 for width in [1440,390,320]:
  page=browser.new_page(viewport={'width':width,'height':844},java_script_enabled=False);page.goto((base/'chart-oracle.html').as_uri())
  result=page.locator('.axr-chart svg').evaluate_all('''svgs=>svgs.map(svg=>({font:parseFloat(getComputedStyle(svg).fontSize)*svg.getBoundingClientRect().width/svg.viewBox.baseVal.width,clipped:[...svg.querySelectorAll('text')].filter(t=>{let a=t.getBoundingClientRect(),b=svg.getBoundingClientRect();return a.left<b.left-1||a.right>b.right+1||a.top<b.top-1||a.bottom>b.bottom+1}).map(t=>t.textContent)}))''')
  check(f'noJS readable fonts {width}',all(x['font']>=12 for x in result),result)
  check(f'noJS labels fit {width}',all(not x['clipped'] for x in result),result)
  page.screenshot(path=str(args.output_dir/f'nojs-{width}.png'),full_page=True)
  page.close()
 browser.close()
(args.output_dir/'chart-fixture-checks.json').write_text(json.dumps(checks,indent=2));print(json.dumps({'checks':len(checks),'failures':[x for x in checks if not x['pass_']]},indent=2))

raise SystemExit(0 if all(x["pass_"] for x in checks) else 1)
