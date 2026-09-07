"""Exercise actual modeling tasks against R-generated answer data.

This is implementer acceptance automation, not a participant usability study.
Requires the fixtures from render-explorer-cases.R, Playwright and axe-core.
"""
import argparse
import hashlib
import os
import re
import importlib.metadata
import platform
import xml.etree.ElementTree as ET
import json
from pathlib import Path
import subprocess
from playwright.sync_api import sync_playwright
from report_geometry import cost_geometry, effect_geometry, importance_geometry

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--case-dir', type=Path, default=Path('/tmp/autoxplain-explorer-cases'))
parser.add_argument('--output-dir', type=Path, default=Path('/tmp/autoxplain-explorer-check'))
parser.add_argument('--axe-path', type=Path, required=True)
parser.add_argument('--cases', nargs='+', default=['regression', 'binary', 'multiclass', 'quick'])
args = parser.parse_args()
args.output_dir.mkdir(parents=True, exist_ok=True)

def settled(page):
    page.evaluate("()=>new Promise(resolve=>requestAnimationFrame(()=>requestAnimationFrame(resolve)))")
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

def check_layout(page, name, artifact):
    settled(page)
    layout = page.evaluate('''() => ({
      viewport: innerWidth, width: document.documentElement.scrollWidth,
      pages: [...document.querySelectorAll('.workspace-page')].filter(el => !el.hidden).length,
      elements: [...document.querySelectorAll('.workspace-page:not([hidden]) *')]
        .filter(el => !el.closest('details:not([open]),.help,.table-wrap,.data-plot-scroll,.selection-chart-wrap,pre'))
        .filter(el => el.getBoundingClientRect().width>0)
        .map(el => ({tag: el.tagName, id: el.id, class: el.className,
          left: el.getBoundingClientRect().left, right: el.getBoundingClientRect().right}))
        .filter(el => el.right > innerWidth || el.left < 0)
    })''')
    labels=page.locator('.axr-chart svg').evaluate_all('''svgs=>svgs.filter(s=>s.getBoundingClientRect().width).flatMap(svg=>[...svg.querySelectorAll('text')].filter(t=>{let a=t.getBoundingClientRect(),b=svg.getBoundingClientRect();return a.left<b.left-1||a.right>b.right+1||a.top<b.top-1||a.bottom>b.bottom+1}).map(t=>t.textContent))''')
    layout['clipped_chart_labels']=labels
    passed = layout['pages'] == 1 and layout['width'] <= layout['viewport'] and not layout['elements'] and not labels
    check(name, passed, None if passed else layout)
    if not passed:
        page.screenshot(path=str(args.output_dir / f'{artifact}.png'), full_page=True)
        (args.output_dir / f'{artifact}.html').write_text(page.content())

with sync_playwright() as playwright:
    options={'executable_path':os.environ['BROWSER_EXECUTABLE']} if os.environ.get('BROWSER_EXECUTABLE') else {}
    browser = playwright.chromium.launch(**options)
    browser_version=browser.version
    for case in args.cases:
        print(f"Checking {case}", flush=True)
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
        for spec in oracle['specifications']:
            row = page.locator(f'[data-model-row="{spec["id"]}"]')
            measurements = next(item for item in oracle['table'] if item['model_id'] == spec['id'])
            costs = row.locator('td.number:visible').all_text_contents()[1:]
            costs_match = len(costs) == len(oracle['resources'])
            for shown, resource in zip(costs, oracle['resources']):
                expected = measurements.get(resource)
                costs_match = costs_match and (shown == 'Unavailable' if expected is None else
                    within_tolerance(0 if shown == '~0' else float(shown), expected))
            check(f'{case}/{spec["id"]}: displayed cost measurements match R', costs_match)
            check(f'{case}/{spec["id"]}: visible fitted settings',
                  row.locator('.model-settings').inner_text() == spec['summary'])
            link = row.locator('[data-open-spec]')
            link.focus()
            page.keyboard.press('Enter')
            dialog = page.get_by_role('dialog')
            check(f'{case}/{spec["id"]}: model details open from keyboard', dialog.is_visible())
            settings = dialog.locator('table').filter(has=page.locator('caption', has_text='Recorded settings, using R parameter names'))
            shown = dict(settings.locator('tbody tr').evaluate_all(
                'rows => rows.map(row => Array.from(row.cells, cell => cell.textContent))'))
            check(f'{case}/{spec["id"]}: detailed settings match R', shown == spec['parameters'])
            page.keyboard.press('Escape')
            check(f'{case}/{spec["id"]}: escape returns focus to model',
                  not dialog.is_visible() and link.evaluate('el => el === document.activeElement'))
        for metric in oracle['metrics']:
            page.select_option('#metric-select', metric)
            settled(page)
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
            for resource in (oracle['resources'] if len(ids)>1 else []):
                page.select_option('#resource-select', resource)
                settled(page)
                plot = page.locator('[data-cost-plot]:visible')
                check(f'{case}/{metric}/{resource}: selected cost plot', plot.count() == 1
                      and plot.get_attribute('data-resource') == resource
                      and plot.get_attribute('data-cost-plot') == metric)
                matched, evidence = cost_geometry(plot, oracle['table'], metric, resource, higher)
                check(f'{case}/{metric}/{resource}: plotted costs, scores and frontier match R', matched, evidence)
        page.select_option('#metric-select', oracle['primary_metric'])
        if oracle['resources'] and page.locator('#resource-select').count():
            page.select_option('#resource-select', oracle['resources'][0])
        settled(page)
        if page.locator('[data-sort=training_time_ms]').count() and len(oracle['resources'])>1:
            page.locator('[data-sort=training_time_ms]').click()
            order=page.locator('[data-model-row]').evaluate_all('xs=>xs.map(x=>x.dataset.modelRow)')
            page.select_option('#resource-select',oracle['resources'][-1]); settled(page)
            check(f'{case}: changing resource preserves manual table order',order==page.locator('[data-model-row]').evaluate_all('xs=>xs.map(x=>x.dataset.modelRow)'))
        for model_id in ids:
            page.locator('[data-page-link=patterns]').click()
            page.select_option('#feature-model-select', model_id)
            settled(page)
            check(f'{case}/{model_id}: feature panel follows model', active_model(page, 'patterns') == model_id)
            spec = next(spec for spec in oracle['specifications'] if spec['id'] == model_id)
            check(f'{case}/{model_id}: selected model settings follow model',
                  page.locator('#patterns [data-model-panel]:visible .model-settings').inner_text() == spec['summary'])
            rows = [row for row in oracle['importance'] if row['model'] == model_id]
            panel = page.locator(f'#patterns [data-model-panel="{model_id}"]')
            if not panel.is_visible():
                continue  # Keep recording a broken model selector without waiting on hidden controls.
            matched,evidence=importance_geometry(panel,rows)
            check(f'{case}/{model_id}: importance bars and shuffle intervals match R',matched,evidence)
            for row in rows:
                button = panel.locator(f'[data-pick-feature="{row["feature"]}"]')
                actual = float(button.locator('strong').inner_text())
                check(f'{case}/{model_id}/{row["feature"]}: importance matches R',
                      within_tolerance(actual, row['importance']))
                button.click()
                settled(page)
                selected = panel.locator('[data-feature-panel]:visible')
                check(f'{case}/{model_id}/{row["feature"]}: selected feature evidence', selected.count() == 1
                      and selected.get_attribute('data-feature-panel') == row['feature'])
                for class_name in (oracle.get('classes') or [None]):
                    if class_name:
                        page.select_option('#effect-class-select', class_name)
                        settled(page)
                        curve_panel = selected.locator('[data-class-panel]:visible')
                        curves = oracle['class_curves'][class_name]
                        check(f'{case}/{model_id}/{row["feature"]}/{class_name}: selected class only',
                              curve_panel.count() == 1 and curve_panel.get_attribute('data-class-panel') == class_name)
                    else:
                        curve_panel = selected
                        curves = oracle.get('curves', {})
                    check(f'{case}/{model_id}/{row["feature"]}/{class_name}: curve or explicit failure',
                          curve_panel.locator('svg').count() == 1 or curve_panel.locator('.empty-state,.diagnostic-state').count() > 0)
                    curve = curves.get(model_id, {}).get(row['feature'])
                    if curve:
                        figure = curve_panel.get_by_role('figure')
                        caption = figure.locator('figcaption').text_content() if figure.count() == 1 else ''
                        method = 'ALE' if 'accumulated_effect' in curve else 'PDP'
                        target = class_name or (oracle['prediction_source'][model_id]['positive']
                                                if oracle['task'] == 'binary' else None)
                        caption_identifies_curve = (method in caption and row['feature'] in caption
                                                    and (target is None or f'`{target}`' in caption))
                        check(f'{case}/{model_id}/{row["feature"]}/{class_name}: visible figure identifies method, input and prediction class',
                              caption_identifies_curve and curve_panel.get_by_role('figure', name=caption, exact=True).count() == 1,
                              caption)
                        shown = curve_panel.locator('table tbody tr').evaluate_all(
                            'rows => rows.map(row => Array.from(row.cells, cell => cell.textContent))')
                        label=next(item['model'] for item in oracle['table'] if item['model_id']==model_id)
                        shown=[row[1:] for row in shown if row and row[0]==label]
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
                        check(f'{case}/{model_id}/{row["feature"]}/{class_name}: curve values match R', matches)
                        matched, evidence = effect_geometry(curve_panel, curve, model_id)
                        check(f'{case}/{model_id}/{row["feature"]}/{class_name}: plotted curve matches its axes and R',
                              matched, evidence)
            page.locator('[data-page-link=evaluation]').click()
            check(f'{case}/{model_id}: prediction panel follows model', active_model(page, 'evaluation') == model_id)
            panel = page.locator(f'#evaluation [data-model-panel="{model_id}"]')
            command=panel.locator('pre').first
            panel.locator('details:has(pre) summary').first.click()
            check(f'{case}/{model_id}: usable R prediction command',
                  command.is_visible() and f'model = "{model_id}"' in command.text_content())
            panel.locator('details:has(pre) summary').first.click()
            expected = oracle.get('predictions',{}).get(model_id)
            if expected:
                metrics=panel.locator('.prediction-metrics').inner_text()
                if oracle['task']=='regression':
                    actual=panel.locator('.prediction-metrics > div').filter(has=page.locator('span',has_text=re.compile(r'^MAE'))).locator('strong').inner_text()
                    check(f'{case}/{model_id}: error matches R',within_tolerance(float(actual),expected['mean_absolute_error']))
                else:
                    # Counts are visible in the confusion table; detailed cutoff/calibration
                    # and individual record checks belong to check-predictions.py.
                    cells=panel.locator('.prediction-confusion tbody tr').evaluate_all('rows=>rows.map(r=>[...r.querySelectorAll("td strong")].map(x=>Number(x.textContent)))')
                    mistakes=sum(value for i,row in enumerate(cells) for j,value in enumerate(row) if i!=j)
                    check(f'{case}/{model_id}: mistakes match R',mistakes==expected['mistakes'])
        page.locator('[data-page-link=patterns]').click(); settled(page)
        page.select_option('#feature-model-select',ids[0]); settled(page)
        prior=page.evaluate('AutoXplainRReport.getState()')
        if len(ids)>1:
            page.select_option('#feature-model-select',ids[1]); settled(page)
            compatible=page.locator('#patterns [data-model-panel]:visible .feature-select option').evaluate_all('xs=>xs.map(x=>x.value)')
            check(f'{case}: preserves compatible input across models',prior['feature'] not in compatible or page.evaluate('AutoXplainRReport.getState().feature')==prior['feature'])
            changed=page.evaluate('AutoXplainRReport.getState().modelId')==ids[1]
            check(f'{case}: model selector changes shared state',changed)
            if changed:
                page.select_option('#comparison-model-select',ids[0]); settled(page)
                comparison = page.locator('#patterns .comparison-identity:visible')
                comparison_spec = next(spec for spec in oracle['specifications'] if spec['id'] == ids[0])
                check(f'{case}: comparison exposes the actual fitted settings',
                      comparison.locator('.comparison-settings').inner_text() == comparison_spec['summary'])
                comparison_link = comparison.locator('[data-open-spec]')
                comparison_link.click()
                check(f'{case}: comparison details open the correct model',
                      page.get_by_role('dialog').is_visible() and comparison_link.get_attribute('data-open-spec') == ids[0]
                      and page.locator('#model-dialog-title').inner_text() == next(row['model'] for row in oracle['table'] if row['model_id'] == ids[0]))
                page.keyboard.press('Escape')
                figure=page.locator('#patterns .axr-chart:visible').first
                if figure.count() and figure.locator(f'[data-chart-source][data-model="{ids[0]}"]').count():
                    check(f'{case}: selected comparison exposes both model curves',set(figure.get_attribute('data-visible-models').split(','))=={ids[0],ids[1]})
                first=page.locator('#patterns [data-chart-point]:visible').first
                if first.count():
                    first.focus(); first.press('ArrowRight')
                    check(f'{case}: keyboard chart inspection keeps readable evidence',page.locator('.axr-chart-tooltip').is_visible() and len(page.locator('.axr-chart-tooltip').inner_text())>20)
                state=page.evaluate('AutoXplainRReport.getState()'); page.reload(); settled(page)
                check(f'{case}: URL restores model feature class and comparison',state==page.evaluate('AutoXplainRReport.getState()'))
                page.select_option('#feature-model-select',ids[0]); settled(page)
                check(f'{case}: comparison never equals primary model',page.evaluate('AutoXplainRReport.getState().comparisonModelId')=='')
        page.locator('[data-page-link=checks]').click(); settled(page)
        link=page.locator('#checks .affected a').first
        if link.count():
            model=link.get_attribute('data-evidence-model'); feature=link.get_attribute('data-evidence-feature')
            link.click(); settled(page)
            state=page.evaluate('AutoXplainRReport.getState()')
            visible=page.evaluate('''()=>{let b=document.activeElement.getBoundingClientRect();return document.activeElement.tagName!=='BODY'&&b.top>=0&&b.top<innerHeight&&b.bottom>0}''')
            check(f'{case}: affected evidence link focuses visible target in correct scope',visible and (not model or state['modelId']==model) and (not feature or state['feature']==feature))
            target_id = page.evaluate('document.activeElement.id')
            page.reload(); settled(page)
            restored=page.evaluate('''id=>{let b=document.activeElement.getBoundingClientRect();return !!id&&document.activeElement.id===id&&b.top>=0&&b.top<innerHeight&&b.bottom>0}''',target_id)
            check(f'{case}: direct evidence URL survives reload',restored and page.evaluate('AutoXplainRReport.getState().modelId')==state['modelId'])
        page.locator('[data-page-link=data]').click(); settled(page)
        check(f'{case}: data exploration workspace available',page.locator('#data').is_visible())
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
        check(f'{case}: keyboard tab navigation', is_visible(page, '#selection'))
        page.locator('[data-page-link=patterns]').click(); settled(page)
        # Only one tab is printed, with the currently inspected model. This is
        # intentionally a view export, not a dump of every hidden combination.
        page.select_option('#feature-model-select', oracle['primary'])
        panel = page.locator('#patterns [data-model-panel]:visible')
        feature_control = panel.locator('.feature-select')
        feature_control.select_option(index=0)
        settled(page)
        page.pdf(path=str(args.output_dir / f'{case}-features.pdf'), print_background=True)
        pdf_text = subprocess.check_output(['pdftotext', '-layout',
            str(args.output_dir / f'{case}-features.pdf'), '-'], text=True)
        pdf_xml=subprocess.check_output(['pdftotext','-bbox',str(args.output_dir/f'{case}-features.pdf'),'-'],text=True)
        printed_words=[(float(word.attrib['yMax'])-float(word.attrib['yMin']),word.text)
            for word in ET.fromstring(pdf_xml).iter() if word.tag.endswith('word')]
        tiny=[(height,word) for height,word in printed_words if height<8]
        check(f'{case}: printed labels remain readable after page reflow',not tiny,tiny)
        check(f'{case}: print includes selected feature view', 'Feature importance' in pdf_text
              and 'Compare the models' not in pdf_text and 'Change in' in pdf_text)
        first_print_page = pdf_text.split('\f')[0]
        check(f'{case}: first printed page contains evidence, not only a heading',
              'Change in' in first_print_page and 'worsens' in first_print_page,
              first_print_page)
        chart_guidance = panel.locator('.axr-chart:visible .axr-chart-guidance p').all_text_contents()
        normalized_print = ' '.join(pdf_text.split())
        missing_guidance = [text for text in chart_guidance
                            if ' '.join(text.split()) not in normalized_print]
        check(f'{case}: print includes collapsed chart methodology',
              not missing_guidance, missing_guidance)
        if oracle.get('classes'):
            check(f'{case}: printed curve retains selected class',
                  page.locator('#effect-class-select').input_value() in pdf_text)
        primary_label = next(row['model'] for row in oracle['table'] if row['model_id'] == oracle['primary'])
        check(f'{case}: print retains report and model identity', primary_label in pdf_text
              and page.locator('h1').inner_text() in pdf_text)
        # Open details on a phone as well as on desktop: the close action must
        # remain reachable while long specifications scroll independently.
        for width in (320, 390, 1440):
            page.set_viewport_size({'width': width, 'height': 844})
            page.locator('[data-page-link=overview]').click()
            page.locator('[data-model-row] [data-open-spec]').first.click()
            dialog = page.get_by_role('dialog')
            check(f'{case}/{width}: details fit viewport', dialog.evaluate('el => { const b=el.getBoundingClientRect(); return b.left>=0 && b.right<=innerWidth && b.top>=0 && b.bottom<=innerHeight; }'))
            dialog.locator('.dialog-content').evaluate('el => el.scrollTop = el.scrollHeight')
            close = dialog.get_by_role('button', name='Close model details')
            check(f'{case}/{width}: close remains visible after scrolling details', close.evaluate('el => { const b=el.getBoundingClientRect(); return b.top>=0 && b.bottom<=innerHeight; }'))
            page.add_script_tag(path=str(args.axe_path.resolve()))
            violations = page.evaluate("async () => (await axe.run(document, {runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21aa','wcag22aa']}})).violations.map(x=>({id:x.id,nodes:x.nodes.map(n=>n.target)}))")
            check(f'{case}/{width}: model details accessibility', not violations, violations)
            if case == 'regression':
                dialog.locator('.dialog-content').evaluate('el => el.scrollTop = 0')
                page.screenshot(path=str(args.output_dir / f'model-details-{width}.png'))
                if width == 1440:
                    page.pdf(path=str(args.output_dir / 'model-details.pdf'), print_background=True)
                    text = subprocess.check_output(['pdftotext', str(args.output_dir / 'model-details.pdf'), '-'], text=True)
                    check('details print keeps fit without background comparison',
                          'Training settings' in text and 'Compare the models' not in text)
            close.click()
        for width in (320, 390, 768, 1440):
            page.set_viewport_size({'width': width, 'height': 1000})
            for tab in ('overview', 'selection', 'data', 'patterns', 'evaluation', 'checks', 'provenance'):
                page.locator(f'[data-page-link={tab}]').click()
                check_layout(page, f'{case}/{width}/{tab}: one tab, no page overflow',
                             f'{case}-{width}-{tab}-overflow')
                check(f'{case}/{width}/{tab}: heading is not hidden by navigation', page.evaluate('''() => {
                  const h = document.querySelector('.workspace-header h1').getBoundingClientRect();
                  const n = document.querySelector('.sidebar').getBoundingClientRect();
                  return h.top >= 0 && (innerWidth > 760 || h.top >= n.bottom);
                }'''))
                if width < 760:
                    check(f'{case}/{width}/{tab}: active mobile tab is visible', page.evaluate('''() => {
                      const tab = document.querySelector('[data-page-link][aria-selected="true"]');
                      const box = tab.getBoundingClientRect(), parent = tab.parentElement.getBoundingClientRect();
                      return box.left >= parent.left - 1 && box.right <= parent.right + 1;
                    }'''))
                    check(f'{case}/{width}/{tab}: numeric plots fit their visible region', page.evaluate('''() =>
                      Array.from(document.querySelectorAll('.axr-chart svg'))
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
        # A wider system font exposed an intrinsic flex-width bug on CI. Keep
        # every model reachable at 320px even when font metrics differ by OS.
        page.set_viewport_size({'width': 320, 'height': 1000})
        font = page.add_style_tag(content='''
          .explorer, .explorer button, .explorer select {font-family: "DejaVu Sans", sans-serif}
          .explorer .guided-note p {font-family: monospace; font-size: 16px}
        ''')
        page.locator('[data-page-link=patterns]').click()
        for model_id in ids:
            page.select_option('#feature-model-select', model_id)
            settled(page)
            check_layout(page, f'{case}/{model_id}: features fit with wider system font at 320px',
                         f'{case}-{model_id}-wide-font-overflow')
        page.locator('[data-page-link=checks]').click()
        check_layout(page, f'{case}: context notes fit with wider system font at 320px',
                     f'{case}-context-wide-font-overflow')
        font.evaluate('el => el.remove()')
        context.close()
        no_js = browser.new_context(java_script_enabled=False, viewport={'width': 390, 'height': 844})
        static = no_js.new_page()
        static.goto(report.as_uri())
        check(f'{case}: no-JavaScript retains every model', static.locator('#patterns [data-model-panel]').count() == len(ids))
        check(f'{case}: no-JavaScript retains model specifications',
              static.locator('.model-spec').count() == len(ids))
        check(f'{case}: no-JavaScript exposes all tabs', static.locator('.workspace-page:visible').count() == 7)
        check(f'{case}: no-JavaScript has no page overflow', static.evaluate(
            'document.documentElement.scrollWidth <= innerWidth'))
        check(f'{case}: no-JavaScript chart labels are readable',static.locator('.axr-chart svg').evaluate_all('''svgs=>svgs.every(s=>parseFloat(getComputedStyle(s).fontSize)*s.getBoundingClientRect().width/s.viewBox.baseVal.width>=12)'''))
        check(f'{case}: no-JavaScript data controls are not presented as usable',
              static.locator('#data button:visible:enabled, #data input:visible:enabled, #data select:visible:enabled').count() == 0)
        data_fallback = static.locator('#data .data-static details')
        check(f'{case}: no-JavaScript data distributions are retained', data_fallback.count() > 0)
        if data_fallback.count():
            data_fallback.first.locator('summary').click()
            check(f'{case}: no-JavaScript distribution table opens natively',
                  data_fallback.first.locator('table').is_visible())
        check(f'{case}: no-JavaScript data scope and ledger remain available',
              static.locator('#data .data-ledger').is_visible() and static.locator('#data #data-export-details').is_visible())
        no_js.close()
    browser.close()
if (args.case_dir/'chart-oracle.html').exists():
    hand=subprocess.run([os.sys.executable,str(Path(__file__).with_name('check-chart-fixture.py')),
        '--case-dir',str(args.case_dir),'--output-dir',str(args.output_dir/'chart-fixture')],capture_output=True,text=True)
    check('independent chart fixture geometry and no-JavaScript readability',hand.returncode==0,hand.stdout+hand.stderr if hand.returncode else None)
else:
    check('independent chart fixture is present',False,'Run validation/render-chart-fixture.R with the same EXPLORER_CASES directory.')
summary = dict(passed=not errors and all(record['passed'] for record in records), checks=records,
               errors=errors, accessibility_incomplete=accessibility,
               runtime=dict(python=platform.python_version(),playwright=importlib.metadata.version('playwright'),browser=browser_version),
               scope='R-oracle task checks and implementer browser review; no recruited participants')
(args.output_dir / 'explorer-checks.json').write_text(json.dumps(summary, indent=2))
print(json.dumps(dict(passed=summary['passed'], checks=len(records), failures=[r for r in records if not r['passed']], errors=errors)))
raise SystemExit(0 if summary['passed'] else 1)
