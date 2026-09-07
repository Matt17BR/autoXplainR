"""Exercise actual modeling tasks against R-generated answer data.

This is implementer acceptance automation, not a participant usability study.
Requires the fixtures from render-explorer-cases.R, Playwright and axe-core.
"""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
from playwright.sync_api import sync_playwright

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--case-dir', type=Path, default=Path('/tmp/autoxplain-explorer-cases'))
parser.add_argument('--output-dir', type=Path, default=Path('/tmp/autoxplain-explorer-check'))
parser.add_argument('--axe-path', type=Path, required=True)
parser.add_argument('--cases', nargs='+', default=['regression', 'binary', 'multiclass', 'quick'])
args = parser.parse_args()
args.output_dir.mkdir(parents=True, exist_ok=True)
records = []
errors = []
accessibility = []

def check(name, passed, evidence=None):
    records.append(dict(name=name, passed=bool(passed), evidence=evidence))

def is_visible(page, selector):
    return page.locator(selector).count() > 0 and page.locator(selector).first.is_visible()

def active_model(page, section):
    return page.locator(f'#{section} [data-model-panel]:visible').get_attribute('data-model-panel')

def within_tolerance(actual, expected):
    return abs(actual - expected) <= max(1e-8, abs(expected) * .0006)

with sync_playwright() as playwright:
    browser = playwright.chromium.launch()
    for case in args.cases:
        report = (args.case_dir / f'{case}.html').resolve()
        oracle = json.loads(report.with_suffix('.json').read_text())
        ids = [row['model_id'] for row in oracle['table']]
        check(f'{case}: fixture identity', True, hashlib.sha256(report.read_bytes()).hexdigest())
        context = browser.new_context(viewport={'width': 1440, 'height': 1000})
        context.route('http://**/*', lambda route: route.abort())
        context.route('https://**/*', lambda route: route.abort())
        page = context.new_page()
        page.on('pageerror', lambda error: errors.append(str(error)))
        page.goto(report.as_uri())
        check(f'{case}: unique IDs and valid navigation targets', page.evaluate('''() => {
          const ids = Array.from(document.querySelectorAll('[id]'), node => node.id);
          return new Set(ids).size === ids.length &&
            Array.from(document.querySelectorAll('[data-page-link]')).every(node =>
              document.getElementById(node.dataset.pageLink));
        }'''))
        check(f'{case}: starts on one comparison tab', page.locator('.workspace-page:visible').count() == 1
              and is_visible(page, '#overview'))
        check(f'{case}: all fitted models visible', set(page.locator('[data-model-row]').evaluate_all(
            'rows => rows.map(row => row.dataset.modelRow)')) == set(ids))
        for metric in oracle['metrics']:
            page.select_option('#metric-select', metric)
            values = []
            for row in oracle['table']:
                selector = f'[data-model-row="{row["model_id"]}"] [data-score-column="{metric}"]'
                shown = page.locator(selector).inner_text()
                expected = row.get(metric)
                check(f'{case}/{metric}/{row["model_id"]}: score matches R',
                      shown == 'Unavailable' if expected is None else within_tolerance(float(shown), expected))
                if expected is not None:
                    values.append(expected)
            higher = metric in ('accuracy', 'auc', 'roc_auc', 'balanced_accuracy', 'r_squared', 'macro_recall')
            first = page.locator('[data-model-row]').first.get_attribute(f'data-value-{metric}')
            check(f'{case}/{metric}: correct score direction', not values or
                  float(first) == (max(values) if higher else min(values)))
            for resource in oracle['resources']:
                page.select_option('#resource-select', resource)
                plot = page.locator('[data-cost-plot]:visible')
                check(f'{case}/{metric}/{resource}: selected cost plot', plot.count() == 1
                      and plot.get_attribute('data-resource') == resource
                      and plot.get_attribute('data-cost-plot') == metric)
        page.select_option('#metric-select', oracle['primary_metric'])
        page.select_option('#resource-select', oracle['resources'][0])
        for model_id in ids:
            page.locator('[data-page-link=patterns]').click()
            page.select_option('#feature-model-select', model_id)
            check(f'{case}/{model_id}: feature panel follows model', active_model(page, 'patterns') == model_id)
            rows = [row for row in oracle['importance'] if row['model'] == model_id]
            panel = page.locator(f'#patterns [data-model-panel="{model_id}"]')
            if not panel.is_visible():
                continue  # Keep recording a broken model selector without waiting on hidden controls.
            for row in rows:
                button = panel.locator(f'[data-pick-feature="{row["feature"]}"]')
                actual = float(button.locator('strong').inner_text())
                check(f'{case}/{model_id}/{row["feature"]}: importance matches R',
                      within_tolerance(actual, row['importance']))
                button.click()
                selected = panel.locator('[data-feature-panel]:visible')
                check(f'{case}/{model_id}/{row["feature"]}: selected feature evidence', selected.count() == 1
                      and selected.get_attribute('data-feature-panel') == row['feature'])
                check(f'{case}/{model_id}/{row["feature"]}: curve or explicit failure',
                      selected.locator('svg').count() == 1 or selected.locator('.empty-state').count() == 1)
                curve = oracle.get('curves', {}).get(model_id, {}).get(row['feature'])
                if curve:
                    shown = selected.locator('table tbody tr').evaluate_all(
                        'rows => rows.map(row => Array.from(row.cells, cell => cell.textContent))')
                    expected_rows = list(zip(*curve.values()))
                    matches = len(shown) == len(expected_rows)
                    for actual_row, expected_row in zip(shown, expected_rows):
                        for actual, expected in zip(actual_row, expected_row):
                            if isinstance(expected, (int, float)):
                                matches = matches and abs(float(actual) - expected) <= .000501
                            elif expected is None:
                                matches = matches and actual in ('Unavailable', 'n/a', 'NA')
                            else:
                                matches = matches and actual == str(expected)
                    check(f'{case}/{model_id}/{row["feature"]}: curve values match R', matches)
            page.locator('[data-page-link=evaluation]').click()
            check(f'{case}/{model_id}: prediction panel follows model', active_model(page, 'evaluation') == model_id)
            panel = page.locator(f'#evaluation [data-model-panel="{model_id}"]')
            check(f'{case}/{model_id}: usable R prediction command',
                  f'model = "{model_id}"' in panel.inner_text())
            expected = oracle['predictions'][model_id]
            summary = panel.locator('.task-intro').inner_text()
            if oracle['task'] == 'regression':
                mae = float(summary.split('Average absolute error: ')[1].split('. ')[0])
                check(f'{case}/{model_id}: error matches R', within_tolerance(mae, expected['mean_absolute_error']))
            else:
                check(f'{case}/{model_id}: mistakes match R',
                      f'{expected["mistakes"]} of {expected["total"]} rows' in summary)
        page.locator('[data-page-link=relationships]').click()
        pairs = oracle['relationships']
        off_diagonal = [pair for pair in pairs if pair['a'] != pair['b'] and pair['value'] is not None]
        strongest = max(off_diagonal, key=lambda pair: abs(pair['value']))
        button = page.get_by_role('button', name=f'{strongest["a"]} and {strongest["b"]}:', exact=False).first
        button.click()
        detail = page.locator('#pair-detail').inner_text()
        check(f'{case}: relationship method and count readable', strongest['method'] in detail
              and f'{strongest["n"]} complete training rows' in detail)
        page.locator('[data-page-link=overview]').click()
        help_button = page.get_by_role('button', name='Score definitions', exact=True)
        help_button.hover()
        tooltip = page.locator('#' + help_button.get_attribute('aria-controls'))
        check(f'{case}: help on hover', tooltip.is_visible() and tooltip.evaluate(
            'el => getComputedStyle(el).visibility') == 'visible')
        help_button.focus()
        check(f'{case}: help on keyboard focus', tooltip.evaluate('el => getComputedStyle(el).visibility') == 'visible')
        help_button.press('Escape')
        check(f'{case}: escape dismisses help', tooltip.evaluate('el => getComputedStyle(el).visibility') == 'hidden')
        help_button.click()
        check(f'{case}: help on tap', help_button.get_attribute('aria-expanded') == 'true')
        page.locator('[data-page-link=overview]').focus()
        page.keyboard.press('ArrowDown')
        check(f'{case}: keyboard tab navigation', is_visible(page, '#patterns'))
        # Only one tab is printed, with the currently inspected model. This is
        # intentionally a view export, not a dump of every hidden combination.
        page.select_option('#feature-model-select', oracle['primary'])
        panel = page.locator('#patterns [data-model-panel]:visible')
        feature_control = panel.locator('.feature-select')
        feature_control.select_option(index=0)
        page.pdf(path=str(args.output_dir / f'{case}-features.pdf'), print_background=True)
        pdf_text = subprocess.check_output(['pdftotext', '-layout',
            str(args.output_dir / f'{case}-features.pdf'), '-'], text=True)
        check(f'{case}: print includes selected feature view', 'Feature importance' in pdf_text
              and 'Compare the models' not in pdf_text and 'Change in' in pdf_text)
        primary_label = next(row['model'] for row in oracle['table'] if row['model_id'] == oracle['primary'])
        check(f'{case}: print retains report and model identity', primary_label in pdf_text
              and page.locator('h1').inner_text() in pdf_text)
        for width in (320, 390, 768, 1440):
            page.set_viewport_size({'width': width, 'height': 1000})
            for tab in ('overview', 'patterns', 'relationships', 'evaluation', 'checks', 'provenance'):
                page.locator(f'[data-page-link={tab}]').click()
                check(f'{case}/{width}/{tab}: one tab, no page overflow',
                      page.locator('.workspace-page:visible').count() == 1 and page.evaluate(
                          'document.documentElement.scrollWidth <= innerWidth'))
                check(f'{case}/{width}/{tab}: heading is not hidden by navigation', page.evaluate('''() => {
                  const h = document.querySelector('.workspace-header h1').getBoundingClientRect();
                  const n = document.querySelector('.sidebar').getBoundingClientRect();
                  return h.top >= 0 && (innerWidth > 760 || h.top >= n.bottom);
                }'''))
                if width < 760:
                    check(f'{case}/{width}/{tab}: numeric plots fit their visible region', page.evaluate('''() =>
                      Array.from(document.querySelectorAll('.tradeoff-plot,.prediction-plot,.effect-plot[data-axis-type="numeric"]'))
                        .filter(el => el.getBoundingClientRect().width > 0)
                        .every(el => el.getBoundingClientRect().width <= el.parentElement.clientWidth + 1)
                    '''))
                if case == 'regression':
                    page.add_script_tag(path=str(args.axe_path.resolve()))
                    axe = page.evaluate('''async () => await axe.run(document, {
                      runOnly: {type: 'tag', values: ['wcag2a','wcag2aa','wcag21aa','wcag22aa']}})''')
                    violations = [{'id': entry['id'], 'nodes': [node['target'] for node in entry['nodes']]}
                                  for entry in axe['violations']]
                    check(f'{width}/{tab}: automated accessibility', not violations, violations)
                    accessibility.append(dict(width=width, tab=tab,
                                              incomplete=[entry['id'] for entry in axe['incomplete']]))
                    page.screenshot(path=str(args.output_dir / f'{tab}-{width}.png'), full_page=True)
        context.close()
        no_js = browser.new_context(java_script_enabled=False, viewport={'width': 390, 'height': 844})
        static = no_js.new_page()
        static.goto(report.as_uri())
        check(f'{case}: no-JavaScript retains every model', static.locator('#patterns [data-model-panel]').count() == len(ids))
        check(f'{case}: no-JavaScript exposes all tabs', static.locator('.workspace-page:visible').count() == 6)
        check(f'{case}: no-JavaScript has no page overflow', static.evaluate(
            'document.documentElement.scrollWidth <= innerWidth'))
        no_js.close()
    browser.close()
summary = dict(passed=not errors and all(record['passed'] for record in records), checks=records,
               errors=errors, accessibility_incomplete=accessibility,
               scope='R-oracle task checks and implementer browser review; no recruited participants')
(args.output_dir / 'explorer-checks.json').write_text(json.dumps(summary, indent=2))
print(json.dumps(dict(passed=summary['passed'], checks=len(records), failures=[r for r in records if not r['passed']], errors=errors)))
raise SystemExit(0 if summary['passed'] else 1)
