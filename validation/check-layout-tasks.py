"""Check report tasks that horizontal-overflow and asset-hash checks cannot cover.

Uses the actual public reports, Chromium and Poppler. Outputs include every tab
at desktop/phone widths and each printed view for independent visual inspection.
This is an agent acceptance check, not evidence from recruited participants.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
import unicodedata
import xml.etree.ElementTree as ET

from playwright.sync_api import sync_playwright


ROOT = Path(__file__).resolve().parents[1]
TABS = ('overview', 'selection', 'data', 'patterns', 'evaluation', 'checks', 'provenance')
REPORTS = ('model-report', 'binary-report', 'multiclass-report')


def normalized(value):
    return ''.join(unicodedata.normalize('NFKC', value).split())


def contains_caption(page, phrase):
    # Poppler can interleave a neighboring chart's text between wrapped caption
    # lines. Reconstruct only adjacent lines at the same left edge: matching
    # scattered words in axes or guidance must not stand in for a missing title.
    target = normalized(phrase)
    lines = sorted(page['text_lines'], key=lambda line: (line['top'], line['left']))
    for first in lines:
        value = normalized(first['text'])
        if not value or not target.startswith(value):
            continue
        previous = first
        while value != target:
            following = [line for line in lines
                         if abs(line['left'] - first['left']) <= 2
                         and line['top'] > previous['top'] + 1
                         and -1 <= line['top'] - previous['bottom']
                         <= max(4, (previous['bottom'] - previous['top']) * .7)]
            if not following:
                break
            previous = following[0]
            value += normalized(previous['text'])
            if not target.startswith(value):
                break
        if value == target:
            return True
    return False


def pdf_pages(path):
    xml = subprocess.check_output(['pdftotext', '-bbox-layout', str(path), '-'], text=True)
    pages = []
    for page in ET.fromstring(xml).findall('.//{*}page'):
        words = list(page.iterfind('.//{*}word'))
        lines = list(page.iterfind('.//{*}line'))
        pages.append(dict(
            text=' '.join(word.text or '' for word in words),
            text_lines=[dict(text=' '.join(word.text or '' for word in line.iterfind('.//{*}word')),
                             left=float(line.attrib['xMin']), top=float(line.attrib['yMin']),
                             bottom=float(line.attrib['yMax'])) for line in lines],
            lines=len(lines), words=len(words),
            extent=(max(float(word.attrib['yMax']) for word in words)
                    - min(float(word.attrib['yMin']) for word in words)) if words else 0,
            height=float(page.attrib['height']),
        ))
    return pages


def caption_orphans(pages, charts):
    orphans = []
    for chart in charts:
        axes = [normalized(chart[key]) for key in ('x', 'y') if chart[key]]
        matches = [i for i, page in enumerate(pages)
                   if contains_caption(page, chart['caption'])]
        if not matches or not any(all(axis in normalized(pages[i]['text']) for axis in axes)
                                  for i in matches):
            orphans.append(dict(chart=chart, caption_pages=matches))
    return orphans


def first_plot(page):
    return page.locator('#data-distribution svg').evaluate('''element => {
      const box = element.getBoundingClientRect();
      return {top: box.top, height: box.height, viewport: innerHeight,
        useful_start_visible: box.top + Math.min(box.height / 2, 160) <= innerHeight};
    }''')


def printed_importance(page):
    return page.locator('#patterns .importance-row:visible').evaluate_all('''rows=>rows.map(row=>{
      const value=row.querySelector('strong'), range=document.createRange();range.selectNodeContents(value);
      const text=range.getBoundingClientRect(), bounds=row.getBoundingClientRect();
      const track=row.querySelector('.importance-track').getBoundingClientRect();
      return {feature:row.dataset.pickFeature, value:value.textContent,
        left:text.left, right:text.right, row_left:bounds.left, row_right:bounds.right,
        track_left:track.left, track_width:track.width,
        contained:text.left>=bounds.left-.5 && text.right<=bounds.right+.5};
    })''')


def fold_error_layout(detail):
    return detail.evaluate('''element=>{
      const table=element.querySelector('table');
      const column=[...table.querySelectorAll('thead th')].findIndex(cell=>cell.textContent==='Error');
      const rows=[...table.querySelectorAll('tbody tr')], cell=rows[0]?.children[column];
      if(!cell) return {readable:false};
      const width=cell.getBoundingClientRect().width, font=parseFloat(getComputedStyle(cell).fontSize);
      const height=Math.max(...rows.map(row=>row.getBoundingClientRect().height));
      const viewport=element.querySelector('.table-wrap').clientWidth;
      return {width,font,max_row_height:height,viewport:innerHeight,table_viewport:viewport,
        readable:width>=font*20 && width<=viewport+1 && height<=innerHeight/4};
    }''')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report-dir', type=Path, default=ROOT / 'pkgdown/assets')
    parser.add_argument('--output-dir', type=Path, default=Path('/tmp/autoxplain-layout-tasks'))
    args = parser.parse_args()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    checks, errors, sources = [], [], {}

    def check(name, passed, evidence=None):
        checks.append(dict(name=name, passed=bool(passed), evidence=evidence))

    def settled(page):
        page.evaluate('()=>new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)))')

    with sync_playwright() as runtime:
        options = {'executable_path': os.environ['BROWSER_EXECUTABLE']} if os.environ.get('BROWSER_EXECUTABLE') else {}
        browser = runtime.chromium.launch(**options)
        version = browser.version
        for name in REPORTS:
            source = (args.report_dir / f'{name}.html').resolve()
            sources[name] = hashlib.sha256(source.read_bytes()).hexdigest()
            for width in (1440, 390):
                page = browser.new_page(viewport={'width': width, 'height': 900}, reduced_motion='reduce')
                page.set_default_timeout(10000)
                page.on('pageerror', lambda error: errors.append(str(error)))
                page.route('http://**/*', lambda route: route.abort())
                page.route('https://**/*', lambda route: route.abort())
                page.goto(source.as_uri())
                page.evaluate('document.fonts.ready')
                for tab in TABS:
                    page.locator(f'[data-page-link="{tab}"]').click()
                    if tab == 'selection' and name == 'model-report':
                        page.locator('#selection-family-filter').select_option('tree')
                    settled(page)
                    page.evaluate('scrollTo(0, 0)')
                    prefix = f'{name}/{width}/{tab}'
                    check(prefix + ': one focused page without horizontal overflow',
                          page.locator('.workspace-page:visible').count() == 1
                          and page.evaluate('document.documentElement.scrollWidth <= innerWidth'))
                    if tab == 'data':
                        columns = page.locator('[data-column-name]').evaluate_all(
                            'nodes=>nodes.map(node=>node.dataset.columnName)')
                        column = 'parcel_kg' if 'parcel_kg' in columns else columns[0]
                        picker = page.locator('#data-column-select')
                        if picker.count() and picker.is_visible():
                            picker.select_option(column)
                        else:
                            page.locator(f'[data-column-name="{column}"]').click()
                        settled(page)
                        page.evaluate('scrollTo(0, 0)')
                        geometry = first_plot(page)
                        check(prefix + ': the initial viewport shows useful distribution evidence',
                              geometry['useful_start_visible'], geometry)
                        if name == 'model-report' and width == 390:
                            injected = page.add_style_tag(content='.data-controls{margin-bottom:100vh!important}')
                            settled(page)
                            bad = first_plot(page)
                            check(prefix + ': excessive setup spacing is rejected',
                                  not bad['useful_start_visible'], bad)
                            injected.evaluate('element=>element.remove()')
                            settled(page)
                        summaries = page.locator('#data details > summary:visible')
                        for index in range(summaries.count()):
                            summary = summaries.nth(index)
                            label = summary.inner_text()
                            summary.focus()
                            page.keyboard.press('Enter')
                            opened = summary.evaluate('element=>element.parentElement.open')
                            text = summary.evaluate('element=>element.parentElement.innerText')
                            check(prefix + ': keyboard reveals ' + label,
                                  opened and len(text) > len(label) + 20)
                            page.keyboard.press('Enter')
                        page.evaluate('scrollTo(0, 0)')
                    if tab == 'checks':
                        failure_links = page.locator('#checks .guided-note a[href^="#selection-detail-"]')
                        for index in range(failure_links.count()):
                            link = failure_links.nth(index)
                            destination = link.get_attribute('href')
                            link.focus()
                            page.keyboard.press('Enter')
                            settled(page)
                            detail = page.locator(destination)
                            evidence = detail.evaluate('''element=>{
                              const table=element.querySelector('table');
                              const column=[...table.querySelectorAll('thead th')]
                                .findIndex(cell=>cell.textContent==='Error');
                              const errors=column<0?[]:[...table.querySelectorAll('tbody tr')]
                                .map(row=>row.children[column].textContent).filter(text=>text.trim());
                              return {open:element.open, hidden:element.hidden,
                                family:element.dataset.selectionFamily,
                                selected:document.querySelector('#selection-family-filter').value,
                                focused:document.activeElement===element.querySelector('summary'),
                                errors, text:element.innerText};
                            }''')
                            check(prefix + ': warning opens the actual failed fold evidence',
                                  detail.is_visible() and evidence['open'] and not evidence['hidden']
                                  and evidence['family'] == evidence['selected'] and evidence['focused']
                                  and bool(evidence['errors'])
                                  and all(error in evidence['text'] for error in evidence['errors']), evidence)
                            shape = fold_error_layout(detail)
                            check(prefix + ': fold errors have readable lines and compact rows', shape['readable'], shape)
                            if width == 390 and index == 0:
                                injected = page.add_style_tag(content='''@media screen{
                                  .selection-candidate .table-wrap table{table-layout:fixed!important;width:760px!important}
                                  .selection-candidate .table-wrap th:last-child,
                                  .selection-candidate .table-wrap td:last-child{
                                    width:24px!important;min-width:0!important;max-width:24px!important}}
                                ''')
                                settled(page)
                                bad = fold_error_layout(detail)
                                check(prefix + ': a crushed error column is rejected', not bad['readable'], bad)
                                injected.evaluate('element=>element.remove()')
                                settled(page)
                            page.screenshot(path=str(args.output_dir / f'{name}-{width}-failure-{index}.png'), full_page=True)
                            if width == 390:
                                detail.locator('.table-wrap').evaluate('''element=>{
                                  element.scrollLeft=element.scrollWidth;
                                  element.scrollIntoView({block:'start'});
                                }''')
                                page.screenshot(path=str(args.output_dir / f'{name}-{width}-failure-errors-{index}.png'))
                            page.locator('[data-page-link="checks"]').click()
                            settled(page)
                            page.evaluate('scrollTo(0, 0)')
                    page.screenshot(path=str(args.output_dir / f'{name}-{width}-{tab}.png'), full_page=True)
                    if width != 1440:
                        continue
                    charts = page.locator(f'#{tab} .axr-chart:visible').evaluate_all('''figures=>figures
                      .filter(figure=>!figure.closest('details:not([open])'))
                      .map(figure=>({caption:figure.querySelector('figcaption').textContent,
                        x:figure.dataset.xLabel || '', y:figure.dataset.yLabel || ''}))''')
                    model_summaries = page.locator('#provenance .model-spec > summary').all_text_contents()
                    distribution_method = page.locator('#data-distribution-note').text_content() if tab == 'data' else ''
                    page.emulate_media(media='print')
                    settled(page)
                    if tab == 'patterns':
                        # The report declares 12 mm margins. Measure print-media
                        # text at A4's actual printable width, then also inspect
                        # the resulting PDF; a desktop print preview is wider.
                        page.set_viewport_size({'width': round((210 - 24) * 96 / 25.4), 'height': 900})
                        settled(page)
                        endpoints = printed_importance(page)
                        check(prefix + ': A4 print keeps importance values inside their rows',
                              bool(endpoints) and all(item['contained'] for item in endpoints), endpoints)
                        if name == 'binary-report':
                            injected = page.add_style_tag(content='@media print{.importance-row strong{transform:translateX(60px)!important}}')
                            settled(page)
                            bad = printed_importance(page)
                            check(prefix + ': overflowing print values are rejected',
                                  bool(bad) and not all(item['contained'] for item in bad), bad)
                            injected.evaluate('element=>element.remove()')
                        page.set_viewport_size({'width': width, 'height': 900})
                        settled(page)
                    output = args.output_dir / f'{name}-{tab}.pdf'
                    page.pdf(path=str(output), format='A4', print_background=True)
                    pages = pdf_pages(output)
                    text = ' '.join(item['text'] for item in pages)
                    orphans = caption_orphans(pages, charts)
                    check(prefix + ': printed chart captions stay with their axes', not orphans, orphans)
                    last = pages[-1]
                    check(prefix + ': no short leftover print page',
                          len(pages) == 1 or last['extent'] >= last['height'] * .25,
                          dict(pages=len(pages), final_lines=last['lines'], final_extent=last['extent']))
                    if tab == 'checks' and page.locator('#uncertainty .uncertainty-method').count():
                        check(prefix + ': interval assumptions survive printing',
                              'independent sampling units' in text and 'fitting and selection uncertainty' in text)
                    if tab == 'data':
                        check(prefix + ': distribution denominators and binning survive printing',
                              normalized(distribution_method) in normalized(text), distribution_method)
                    if tab == 'provenance':
                        missing = [summary for summary in model_summaries if normalized(summary) not in normalized(text)]
                        check(prefix + ': printed Methods retains each model summary', not missing, missing)
                    if name == 'model-report' and tab == 'evaluation':
                        # Make the actual browser print an orphan title, preserving every
                        # chart value. This is the defect observed in the old residual view.
                        page.evaluate('''()=>{
                          const section=document.querySelector('#evaluation');
                          const figure=[...section.querySelectorAll('.axr-chart')]
                            .filter(e=>e.getBoundingClientRect().height>0).at(-1);
                          const placeholder=document.createElement('span');
                          placeholder.id='layout-orphan-placeholder';figure.before(placeholder);
                          const caption=figure.querySelector('figcaption');
                          const wrapper=document.createElement('div');wrapper.id='layout-orphan-mutation';
                          const lead=document.createElement('div');lead.style.breakBefore='page';
                          lead.append(caption);wrapper.append(lead);
                          const body=document.createElement('div');body.style.breakBefore='page';
                          body.append(figure);wrapper.append(body);section.append(wrapper);
                        }''')
                        mutant = args.output_dir / 'orphan-title-negative-control.pdf'
                        page.pdf(path=str(mutant), format='A4', print_background=True)
                        check(prefix + ': a deliberately orphaned chart title is rejected',
                              bool(caption_orphans(pdf_pages(mutant), [charts[-1]])))
                        page.evaluate('''()=>{
                          const wrapper=document.querySelector('#layout-orphan-mutation');
                          const figure=wrapper.querySelector('figure');
                          figure.prepend(wrapper.querySelector('figcaption'));
                          document.querySelector('#layout-orphan-placeholder').replaceWith(figure);
                          wrapper.remove();
                        }''')
                    page.emulate_media(media='screen')
                    settled(page)
                page.close()
        browser.close()
    result = dict(passed=not errors and all(item['passed'] for item in checks), checks=checks,
                  browser_errors=errors, browser_version=version, source_sha256=sources)
    (args.output_dir / 'layout-task-checks.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(dict(passed=result['passed'], checks=len(checks),
                          failed=[item for item in checks if not item['passed']], errors=errors), indent=2))
    return 0 if result['passed'] else 1


if __name__ == '__main__':
    raise SystemExit(main())
