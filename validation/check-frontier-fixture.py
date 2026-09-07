"""Check actual SVG against known answers, including deliberately invalid frontiers.

From the repository root:
    EXPLORER_CASES=/path/to/cases Rscript validation/render-frontier-fixture.R
    python validation/check-frontier-fixture.py --case-dir /path/to/cases --output-dir /path/to/checks

The normal chart fixture gate runs both steps automatically in report browser CI.
"""
import argparse, json, os, re
from pathlib import Path
from playwright.sync_api import sync_playwright

from report_geometry import calibration, cost_geometry

parser = argparse.ArgumentParser(description="Verify discrete Pareto steps against literal measurements and visible numeric ticks.")
parser.add_argument('--case-dir', type=Path, default=Path('/tmp/autoxplain-explorer-cases'))
parser.add_argument('--output-dir', type=Path, required=True)
args = parser.parse_args()
base, out = args.case_dir, args.output_dir
out.mkdir(parents=True, exist_ok=True)
cases = json.loads((base / 'frontier-source.json').read_text())
checks = []
def check(label, passed, evidence=None):
    checks.append(dict(check=label, passed=bool(passed), evidence=evidence))
def settled(page):
    page.evaluate('()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve)))')
def path_matches(figure, expected):
    line = figure.locator('.axr-frontier')
    if len(expected) < 2:
        return line.count() == 0, dict(paths=line.count())
    if line.count() != 1:
        return False, dict(paths=line.count())
    actual = line.evaluate('line=>[...line.points].map(point=>[point.x,point.y])')
    if len(actual) != len(expected):
        return False, dict(vertices=len(actual), expected=len(expected))
    try:
        x, xs = calibration(figure.locator('svg'), True)
        y, ys = calibration(figure.locator('svg'))
    except (ValueError, ZeroDivisionError) as error:
        return False, str(error)
    decoded = [[x(px), y(py)] for px, py in actual]
    valid = all(abs(a[0]-e[0]) < abs(xs)*.03 and abs(a[1]-e[1]) < abs(ys)*.03
                for a, e in zip(decoded, expected))
    return valid, decoded

with sync_playwright() as p:
    browser = p.chromium.launch(**({'executable_path': os.environ['BROWSER_EXECUTABLE']}
                                   if os.environ.get('BROWSER_EXECUTABLE') else {}))
    for width in [1440, 390, 320]:
        for enabled in [True, False]:
            page = browser.new_page(viewport=dict(width=width, height=1000), java_script_enabled=enabled)
            errors = []
            page.on('pageerror', lambda error: errors.append(str(error)))
            page.goto((base / 'frontier-oracle.html').as_uri())
            if enabled:
                settled(page)
            for name, case in cases.items():
                fig = page.locator(f'[data-review-case="{name}"] .axr-chart')
                valid, evidence = cost_geometry(fig, case['rows'], case['metric'], case['resource'], case['higher'])
                check(f'{name} points, axes, membership, names {width} JS={enabled}', valid, evidence)
                expected = case['path']
                valid, evidence = path_matches(fig, expected)
                check(f'{name} independently specified frontier vertices {width} JS={enabled}', valid, evidence)
                clipping = fig.locator('svg').evaluate('''svg=>[...svg.querySelectorAll('text')].filter(text=>{
                    const a=text.getBoundingClientRect(), b=svg.getBoundingClientRect();
                    return a.left<b.left-1||a.right>b.right+1||a.top<b.top-1||a.bottom>b.bottom+1;
                }).map(text=>text.textContent)''')
                check(f'{name} tick and model labels fit {width} JS={enabled}', not clipping, clipping)
                if name == 'near_tie':
                    labels = fig.locator('[data-chart-point]').evaluate_all('nodes=>nodes.map(node=>({id:node.dataset.modelId,label:node.getAttribute("aria-label")}))')
                    for row in case['rows']:
                        label = next(item['label'] for item in labels if item['id'] == row['model_id'])
                        match = re.search(r'; RMSE: ([^;]+)', label)
                        check(f'near-tied score can be distinguished on inspection {width} JS={enabled} {row["model_id"]}',
                              match is not None and abs(float(match.group(1)) - row['rmse']) < 1e-15, label)
                if name == 'duplicate_frontier' and enabled:
                    for model_id in ['A', 'B']:
                        fig.locator(f'[data-chart-point][data-model-id="{model_id}"]').focus()
                        expected_label = next(row['model'] for row in case['rows'] if row['model_id'] == model_id)
                        check(f'coincident models remain individually inspectable by keyboard {width} {model_id}',
                              fig.locator('.axr-chart-detail').inner_text().startswith(expected_label + ';'))
                if name in ['loss_ties', 'r2_negative'] and width in [1440, 390]:
                    fig.screenshot(path=str(out / f'{name}-{width}-js-{enabled}.png'))
            check(f'no browser errors {width} JS={enabled}', not errors, errors)
            page.close()
    page = browser.new_page()
    page.goto((base / 'frontier-oracle.html').as_uri())
    settled(page)
    fig = page.locator('[data-review-case="loss_ties"] .axr-chart')
    line = fig.locator('.axr-frontier')
    original = line.get_attribute('points')
    expected = cases['loss_ties']['path']
    line.evaluate("line=>line.setAttribute('points',[...line.points].filter((_,i)=>i%2===0).map(p=>`${p.x},${p.y}`).join(' '))")
    valid, evidence = path_matches(fig, expected)
    check('diagonal interpolation is rejected', not valid, evidence)
    line.evaluate('(line,points)=>line.setAttribute("points",points)', original)
    line.evaluate("line=>{const p=[...line.points].map(p=>[p.x,p.y]);for(let i=1;i<p.length;i+=2)p[i]=[p[i-1][0],p[i+1][1]];line.setAttribute('points',p.map(p=>p.join(',')).join(' '))}")
    valid, evidence = path_matches(fig, expected)
    check('improvement before cost is affordable is rejected', not valid, evidence)
    line.evaluate('(line,points)=>line.setAttribute("points",points)', original)
    line.evaluate("line=>{const p=[...line.points];line.setAttribute('points',p.map(p=>`${p.x+4},${p.y}`).join(' '))}")
    valid, evidence = path_matches(fig, expected)
    check('shifted frontier at correct points is rejected', not valid, evidence)
    page.close()
    browser.close()

(out / 'frontier-checks.json').write_text(json.dumps(checks, indent=2))
failures = [item for item in checks if not item['passed']]
print(json.dumps(dict(checks=len(checks), failures=failures), indent=2))
raise SystemExit(bool(failures))
