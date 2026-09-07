from pathlib import Path
import json, math, argparse, os
from report_geometry import effect_geometry, cost_geometry, label_collisions
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
 dense_source=json.loads((base/'dense-chart-source.json').read_text())
 for width in [1440,390,320]:
  for enabled in [True,False]:
   page=browser.new_page(viewport={'width':width,'height':1050},java_script_enabled=enabled)
   page.goto((base/'dense-chart-oracle.html').as_uri())
   if enabled:settled(page)
   for i,name in enumerate(['dense','near','timing']):
    resource='prediction_time_ms' if name=='timing' else 'model_size_kb'
    figure=page.locator('[data-kind=cost]').nth(i);svg=figure.locator('svg')
    matched,evidence=cost_geometry(figure,dense_source[name],'rmse',resource,False)
    check(f'{name} cost geometry and direct label separation width={width} JS={enabled}',matched,evidence)
    bounds=svg.evaluate('''svg=>({font:Math.min(...[...svg.querySelectorAll('.axr-model-label')].map(t=>
      parseFloat(getComputedStyle(t).fontSize)*svg.getBoundingClientRect().width/svg.viewBox.baseVal.width)),
      clipped:[...svg.querySelectorAll('text')].filter(t=>{const a=t.getBoundingClientRect(),b=svg.getBoundingClientRect();
      return a.left<b.left-1||a.right>b.right+1||a.top<b.top-1||a.bottom>b.bottom+1}).map(t=>t.textContent)})''')
    check(f'{name} readable unclipped labels width={width} JS={enabled}',bounds['font']>=11.5 and not bounds['clipped'],bounds)
    if enabled:
     for model in dense_source[name]:
      group=figure.locator(f'[data-chart-point][data-model-id="{model["model_id"]}"]');group.focus()
      detail=figure.locator('.axr-chart-detail').inner_text()
      highlighted=figure.locator('.axr-model-label.axr-highlight').all_text_contents()
      check(f'{name} keyboard connects exact measurement to label width={width} {model["model_id"]}',
        model['model'] in detail and str(model[resource]) in detail and
        [''.join(x.split()) for x in highlighted]==[''.join(model['model'].split())])
   page.screenshot(path=str(args.output_dir/f'dense-{width}-js-{enabled}.png'),full_page=True)
   if enabled and width==1440:
    page.emulate_media(media='print');page.evaluate("window.dispatchEvent(new Event('beforeprint'))");settled(page)
    for i,name in enumerate(['dense','near','timing']):
     figure=page.locator('[data-kind=cost]').nth(i)
     resource='prediction_time_ms' if name=='timing' else 'model_size_kb'
     matched,evidence=cost_geometry(figure,dense_source[name],'rmse',resource,False)
     check(f'{name} print preserves geometry and separated labels',matched,evidence)
    page.pdf(path=str(args.output_dir/'dense-charts.pdf'),format='A4',print_background=True)
   page.close()
 # A browser may substitute a wider system font than the one used during local
 # development. This changes glyph widths without changing the SVG or answers.
 wider_font='.axr-chart .axr-model-label {font-family:monospace !important;font-weight:700 !important;letter-spacing:.25px !important;}'
 for fixture in ['chart-oracle','dense-chart-oracle']:
  wide=args.output_dir/f'{fixture}-wide-font.html'
  wide.write_text((base/f'{fixture}.html').read_text().replace('</style>',wider_font+'</style>'))
  for width in [1440,390,320]:
   page=browser.new_page(viewport={'width':width,'height':1050},java_script_enabled=False)
   page.goto(wide.resolve().as_uri())
   for i,figure in enumerate(page.locator('[data-kind=cost]').all()):
    svg=figure.locator('svg')
    bounds=svg.evaluate('''svg=>({fonts:[...svg.querySelectorAll('.axr-model-label')].map(t=>getComputedStyle(t).font),
      clipped:[...svg.querySelectorAll('.axr-model-label')].filter(t=>{const a=t.getBoundingClientRect(),b=svg.getBoundingClientRect();
      return a.left<b.left-1||a.right>b.right+1||a.top<b.top-1||a.bottom>b.bottom+1}).map(t=>t.textContent)})''')
    check(f'{fixture}/{i} wide fallback font remains inside static chart width={width}',
          not bounds['clipped'] and all('monospace' in font and '14px' in font for font in bounds['fonts']),bounds)
    check(f'{fixture}/{i} wide fallback font labels stay separated width={width}',not label_collisions(svg))
    if fixture=='dense-chart-oracle':
     name=['dense','near','timing'][i];resource='prediction_time_ms' if name=='timing' else 'model_size_kb'
     matched,evidence=cost_geometry(figure,dense_source[name],'rmse',resource,False)
     check(f'{name} wide fallback font preserves numeric geometry width={width}',matched,evidence)
   if width==390:
    page.locator('[data-kind=cost]').first.screenshot(path=str(args.output_dir/f'{fixture}-wide-font-390.png'))
   page.close()
 # The collision oracle must reject the original failure, even when all numeric
 # points and model identities remain correct.
 page=browser.new_page();page.goto((base/'dense-chart-oracle.html').as_uri());settled(page)
 figure=page.locator('[data-kind=cost]').first
 figure.locator('.axr-model-label').evaluate_all('''labels=>labels.forEach(label=>{
   label.setAttribute('x','160');label.setAttribute('y','150');
   label.querySelectorAll('tspan').forEach(span=>span.setAttribute('x','160'));
 })''')
 rejected,evidence=cost_geometry(figure,dense_source['dense'],'rmse','model_size_kb',False)
 check('deliberately overlapping labels are rejected independently of correct numeric geometry',
       not rejected and isinstance(evidence,dict) and bool(evidence.get('overlapping_model_labels')),evidence)
 page.close();browser.close()
(args.output_dir/'chart-fixture-checks.json').write_text(json.dumps(checks,indent=2));print(json.dumps({'checks':len(checks),'failures':[x for x in checks if not x['pass_']]},indent=2))

raise SystemExit(0 if all(x["pass_"] for x in checks) else 1)
