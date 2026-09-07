"""Capture readable report workspaces, with complete pages retained separately for QA.

README images show a real browser viewport around one task. They are not stitched
full-page posters: those shrink the text and leave a fixed sidebar ending halfway
down the image. No styles, font sizes or report content are changed for capture.
"""
import argparse
import json
import math
import os
from pathlib import Path
import shutil
import tempfile

from playwright.sync_api import sync_playwright


ROOT = Path(__file__).resolve().parents[1]
WIDTH = 1280
MAX_HEIGHT = 1080
SCALE = 1.5
# Each endpoint includes the whole task shown in its README caption. The height
# cap is a framing heuristic, not a usability score. Long pages continue below
# these previews; their complete captures go to --qa-dir.
FRAMES = {
    'overview': {
        'names': ('guided-overview.png', 'model-comparison.png'),
        'end': '.compare-layout',
        'required': ('.model-table-wrap', '.comparison-chart'),
        'purpose': 'Every retained model and the complete performance/cost panel.',
    },
    'selection': {
        'names': ('model-selection.png',),
        'start': '.selection-family-overview',
        'end': '.selection-chart-wrap',
        'required': ('.selection-family-overview', '.selection-filter',
                     '.selection-rationale', '.selection-chart-wrap'),
        'purpose': 'Family comparison, decision-tree search rationale and all seven settings.',
    },
    'data': {
        'names': ('model-data.png',),
        'end': '.data-values',
        'required': ('.data-controls', '.data-column-list', '#data-summary',
                     '.data-view-controls', '#data-distribution', '.data-values'),
        'purpose': 'The selected parcel-weight profile and complete training/evaluation distribution.',
    },
    'patterns': {
        'names': ('model-patterns.png',),
        'end': '.feature-layout',
        'required': ('.feature-controls', '.comparison-identity', '.feature-layout'),
        'purpose': 'Model settings, importance and the complete two-model fitted curve with support.',
    },
    'evaluation': {
        'names': ('model-predictions.png',),
        'end': '.prediction-chart-grid',
        'required': ('.model-control', '.prediction-metrics', '.prediction-chart-grid'),
        'purpose': 'Selected-model scores, observed versus predicted values, error distribution and residual pattern.',
    },
    'checks': {
        'names': ('explanation-reliability.png',),
        'end': '#uncertainty',
        'required': ('#reliability', '#uncertainty'),
        'purpose': 'Actionable findings and their complete score-uncertainty evidence.',
    },
}


def settle(page):
    page.evaluate('()=>new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)))')


def visible(page, tab, selector):
    locator = page.locator(f'#{tab} {selector}').filter(visible=True)
    if locator.count() != 1:
        raise AssertionError(f'{tab}: expected one visible {selector}, found {locator.count()}')
    return locator


def edge_text(page):
    """Find text cut by the outer image edge, excluding intentionally closed details."""
    return page.locator('.report-body').evaluate('''root => {
      const walker = document.createTreeWalker(root, NodeFilter.SHOW_TEXT), cut = [];
      while (walker.nextNode()) {
        const node = walker.currentNode, parent = node.parentElement;
        if (!node.textContent.trim() || !parent.checkVisibility()) continue;
        if (parent.closest('[hidden],.help-tip,.sr-only,[data-chart-source]')) continue;
        let concealed = false;
        for (let el = parent; el && el !== root; el = el.parentElement) {
          if (el.tagName === 'DETAILS' && !el.open && !el.querySelector(':scope > summary')?.contains(parent)) {
            concealed = true; break;
          }
        }
        if (concealed) continue;
        const range = document.createRange(); range.selectNodeContents(node);
        for (const rect of range.getClientRects()) {
          if (rect.width < 1 || rect.height < 1 || rect.right <= 0 || rect.left >= innerWidth) continue;
          if ((rect.top < -0.5 && rect.bottom > 0.5) ||
              (rect.top < innerHeight - 0.5 && rect.bottom > innerHeight + 0.5)) {
            cut.push({text: node.textContent.trim().slice(0, 100), top: rect.top, bottom: rect.bottom});
          }
        }
      }
      return cut;
    }''')


def frame(page, tab, spec):
    # Set the viewport itself, rather than clipping a tall screenshot. This keeps
    # the fixed navigation rail intact and exercises the actual responsive page.
    page.set_viewport_size({'width': WIDTH, 'height': 900})
    page.evaluate('scrollTo({top: 0, behavior: "instant"})')
    settle(page)
    start = 0
    if spec.get('start'):
        start = max(0, math.floor(visible(page, tab, spec['start']).bounding_box()['y']) - 8)
    end = visible(page, tab, spec['end']).bounding_box()
    height = math.ceil(end['y'] + end['height'] - start + 4)
    if not 450 <= height <= MAX_HEIGHT:
        raise AssertionError(
            f'{tab}: the complete task needs {height}px at {WIDTH}px width; '
            f'fix the layout or choose a coherent task within 450–{MAX_HEIGHT}px. '
            'Do not shrink the type or crop required evidence to make capture pass.')
    page.set_viewport_size({'width': WIDTH, 'height': height})
    page.evaluate('(y)=>scrollTo({top: y, behavior: "instant"})', start)
    settle(page)
    geometry = dict(tab=tab, purpose=spec['purpose'], viewport=dict(width=WIDTH, height=height),
                    scroll_y=page.evaluate('scrollY'), required=[])
    for selector in spec['required']:
        rect = visible(page, tab, selector).bounding_box()
        inside = (rect['x'] >= -0.5 and rect['y'] >= -0.5 and
                  rect['x'] + rect['width'] <= WIDTH + 0.5 and
                  rect['y'] + rect['height'] <= height + 0.5)
        geometry['required'].append(dict(selector=selector, bounds=rect, fully_visible=inside))
        if not inside:
            raise AssertionError(f'{tab}: capture would cut required evidence {selector}: {rect}')
    rail = page.locator('.sidebar').bounding_box()
    if rail['y'] != 0 or abs(rail['height'] - height) > 1:
        raise AssertionError(f'{tab}: fixed navigation does not cover the viewport: {rail}')
    if page.evaluate('document.documentElement.scrollWidth > innerWidth'):
        raise AssertionError(f'{tab}: horizontal page overflow')
    cut = edge_text(page)
    if cut:
        raise AssertionError(f'{tab}: screenshot edge cuts text: {cut}')
    geometry['edge_text_cuts'] = cut
    return geometry


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--report', type=Path, default=ROOT / 'pkgdown/assets/model-report.html')
    parser.add_argument('--output-dir', type=Path, default=ROOT / 'man/figures')
    parser.add_argument('--qa-dir', type=Path,
                        default=Path(tempfile.gettempdir()) / 'autoxplain-gallery-capture')
    args = parser.parse_args()
    args.qa_dir.mkdir(parents=True, exist_ok=True)
    errors, records = [], []
    with tempfile.TemporaryDirectory(prefix='autoxplain-gallery-') as staging:
        staged = Path(staging)
        with sync_playwright() as playwright:
            options = {'headless': True}
            if os.environ.get('CHROME_PATH'):
                options['executable_path'] = os.environ['CHROME_PATH']
            browser = playwright.chromium.launch(**options)
            page = browser.new_page(viewport={'width': WIDTH, 'height': 900},
                                    device_scale_factor=SCALE, reduced_motion='reduce')
            page.on('pageerror', lambda error: errors.append(str(error)))
            page.goto(args.report.resolve().as_uri(), wait_until='load')
            page.evaluate('document.fonts.ready')
            for tab, spec in FRAMES.items():
                page.locator(f'[data-page-link={tab}]').click()
                if tab == 'selection':
                    page.locator('#selection-family-filter').select_option('tree')
                if tab == 'patterns':
                    page.locator('#comparison-model-select').select_option(label='Neural network')
                if tab == 'data':
                    page.locator('[data-column-name="parcel_kg"]').click()
                page.mouse.move(WIDTH - 8, 8)
                settle(page)
                assert page.locator('.workspace-page:visible').count() == 1
                page.evaluate('scrollTo({top: 0, behavior: "instant"})')
                page.screenshot(path=str(args.qa_dir / f'{tab}-complete-page.png'),
                                full_page=True, animations='disabled')
                try:
                    record = frame(page, tab, spec)
                except AssertionError:
                    page.screenshot(path=str(args.qa_dir / f'{tab}-framing-failure.png'), animations='disabled')
                    raise
                records.append(record)
                for name in spec['names']:
                    page.screenshot(path=str(staged / name), animations='disabled')
            page.set_viewport_size({'width': WIDTH, 'height': 900})
            page.locator('[data-page-link=overview]').click()
            page.evaluate('scrollTo({top: 0, behavior: "instant"})')
            tree = page.locator('.model-table tbody tr').filter(
                has=page.get_by_role('link', name='Decision tree', exact=True))
            tree.locator('[data-open-spec]').click()
            assert page.get_by_role('dialog').is_visible()
            page.screenshot(path=str(staged / 'model-details.png'), animations='disabled')
            page.keyboard.press('Escape')
            page.set_viewport_size({'width': 390, 'height': 844})
            for tab in FRAMES:
                page.locator(f'[data-page-link={tab}]').click()
                settle(page)
                assert page.evaluate('document.documentElement.scrollWidth <= innerWidth'), tab
            if errors:
                raise AssertionError(f'Browser errors during capture: {errors}')
            evidence = dict(browser=browser.version, report=str(args.report.resolve()),
                            device_scale_factor=SCALE, frames=records, browser_errors=errors,
                            mobile_overflow_checks=len(FRAMES))
            (args.qa_dir / 'capture-geometry.json').write_text(json.dumps(evidence, indent=2) + '\n')
            # Review the actual PNG at a typical GitHub README content width.
            # These are browser-rendered image previews, never edited source captures.
            preview = browser.new_page(viewport={'width': 896, 'height': 1100}, device_scale_factor=1)
            for source in staged.glob('*.png'):
                preview.goto(source.as_uri())
                preview.locator('img').evaluate('(img)=>{img.style.width="896px";img.style.height="auto";img.style.maxWidth="none"}')
                preview.locator('img').screenshot(path=str(args.qa_dir / f'github-width-{source.name}'))
            browser.close()
        # A failed later frame must not leave the committed gallery half-refreshed.
        args.output_dir.mkdir(parents=True, exist_ok=True)
        for source in staged.glob('*.png'):
            shutil.copyfile(source, args.output_dir / source.name)
    print(f'Captured {len(FRAMES)} focused report workspaces and model details in {args.output_dir}. '
          f'Complete pages, GitHub-width previews and geometry: {args.qa_dir}. '
          'All required task elements fit; no cut text, browser errors or 390px page overflow.')


if __name__ == '__main__':
    main()
