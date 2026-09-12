"""Verify exact inert-text transport with independent static and browser reads."""
import argparse
import json
import re
from pathlib import Path
import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[2]))
from report_payload import read_json_text, read_json_payload, replace_json_payload
from browser_runtime import launch
from playwright.sync_api import sync_playwright

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--folder', type=Path, required=True)
parser.add_argument('--browsers', nargs='+', default=['chromium'])
parser.add_argument('--report', type=Path)
args = parser.parse_args()
folder = args.folder.resolve()
html = (folder / 'boundary.html').read_text()
expected = json.loads((folder / 'source.json').read_text())
checks = []
def check(name, passed):
    checks.append(dict(name=name, passed=bool(passed)))

for size in (1, 7, 13, 32):
    check(f'Python: exact escaped text at boundary {size}', read_json_payload(html, f'edge-{size}') == expected)
for kind in ('owner', 'order', 'missing'):
    try:
        read_json_text(html, kind)
    except ValueError:
        check(f'Python: rejects {kind} mismatch', True)
    else:
        check(f'Python: rejects {kind} mismatch', False)
mutated = replace_json_payload(html, 'edge-7', {'deliberate': 'changed <value>'})
check('Python: replacement removes old chunks and preserves deliberate mutation',
      read_json_payload(mutated, 'edge-7') == {'deliberate': 'changed <value>'})
(folder / 'replaced.html').write_text(mutated)
report_mutations = {}
if args.report:
    report_html = args.report.read_text()
    original_data = read_json_payload(report_html, 'axr-data-payload')
    marker = re.search(r'id="axr-data-payload" data-json-chunks="(\d+)"', report_html)
    assert marker and int(marker[1]) > 1, 'Expected naturally chunked report'
    for kind in ('owner', 'order', 'missing'):
        if kind == 'owner':
            altered = report_html.replace('data-json-owner="axr-data-payload"', 'data-json-owner="axr-predictions-payload"', 1)
        elif kind == 'order':
            altered = report_html.replace('data-json-chunk="1"', 'data-json-chunk="2"', 1)
        else:
            altered = re.sub(r'<script type="application/octet-stream" data-json-owner="axr-data-payload"[^>]*>.*?</script>', '', report_html, count=1, flags=re.S)
        target = folder / f'report-{kind}.html'
        target.write_text(altered)
        report_mutations[kind] = target
with sync_playwright() as runtime:
    for engine in args.browsers:
        browser = launch(runtime, engine)
        page = browser.new_page()
        errors = []
        page.on('pageerror', lambda error: errors.append(str(error)))
        page.goto((folder / 'boundary.html').as_uri())
        for size in (1, 7, 13, 32):
            check(f'{engine}: exact Unicode/escaped-text boundary {size}',
                  json.loads(page.locator(f'#edge-{size}').text_content()) == expected)
            check(f'{engine}: successful chunks {size} removed',
                  page.locator(f'[data-json-owner="edge-{size}"]').count() == 0)
        for kind in ('owner', 'order', 'missing'):
            check(f'{engine}: rejects {kind} mismatch', page.locator('#' + kind).text_content() == '')
        check(f'{engine}: hostile text stays inert', page.evaluate('typeof window.pwned') == 'undefined')
        check(f'{engine}: no runtime error', not errors)
        page.goto((folder / 'replaced.html').as_uri())
        check(f'{engine}: deliberate mutation survives old assembler',
              json.loads(page.locator('#edge-7').text_content()) == {'deliberate': 'changed <value>'})
        check(f'{engine}: replacement leaves another payload unchanged',
              json.loads(page.locator('#edge-13').text_content()) == expected)
        if args.report:
            page.goto(args.report.resolve().as_uri())
            scores = page.locator('[data-model-row]').all_text_contents()
            prediction_text = page.locator('#axr-predictions-payload').text_content()
            assert scores and all(text.strip() for text in scores), 'Original report needs visible model rows'
            assert json.loads(prediction_text)['models'], 'Original report needs prediction models'
            for kind, path in report_mutations.items():
                page.goto(path.as_uri())
                page.locator('[data-page-link="data"]').click()
                check(f'{engine}: {kind} mismatch shows visible data fallback',
                      page.locator('[data-decode-error]').is_visible() and not page.locator('.data-workspace').is_visible())
                fallback = page.locator('.data-static details').first
                fallback.locator('summary').click()
                total = fallback.locator('tbody tr').evaluate_all("rows=>rows.reduce((sum,row)=>sum+Number(row.cells[1].textContent.replaceAll(',',''))+Number(row.cells[2].textContent.replaceAll(',','')),0)")
                check(f'{engine}: {kind} mismatch retains full distribution counts', total == original_data['rows']['length'])
                check(f'{engine}: {kind} mismatch leaves prediction payload and scores unchanged',
                      page.locator('#axr-predictions-payload').text_content() == prediction_text and
                      page.locator('[data-model-row]').all_text_contents() == scores)
            check(f'{engine}: corrupted transport has no uncaught errors', not errors)
            context = browser.new_context(java_script_enabled=False)
            static = context.new_page()
            static.goto(args.report.resolve().as_uri() + '#data')
            check(f'{engine}: chunked report works as static evidence without JavaScript',
                  static.locator('[data-model-row]').all_text_contents() == scores and
                  static.locator('.data-static details').count() > 0)
            context.close()
        browser.close()
result = dict(passed=all(check['passed'] for check in checks), checks=checks)
(folder / 'checks.json').write_text(json.dumps(result, indent=2))
print(json.dumps({'checks': len(checks), 'failures': [check for check in checks if not check['passed']]}))
raise SystemExit(0 if result['passed'] else 1)
