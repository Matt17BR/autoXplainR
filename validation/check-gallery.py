"""Bind the committed public gallery to its source files and smoke-test its actual HTML.

--record is an explicit release operation after regenerating reports and screenshots.
CI must run normal mode before any report generator overwrites committed assets.
"""
import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import sys

PUBLIC_REPORTS = ('model-report.html', 'binary-report.html', 'multiclass-report.html')
GENERATORS = ('validation/render-example.R', 'validation/render-explorer-cases.R',
              'validation/capture-screenshots.py', 'validation/check-gallery.py')
REBUILD = ('Regenerate the public reports and screenshots, review them, then explicitly run '
           '`python validation/check-gallery.py --record`. See validation/README.md#report-screenshots. '
           'Do not record a manifest in CI or use recording to skip regeneration.')


def inventory(root):
    sources = set(root.glob('R/**/*.R')) | {path for path in root.glob('inst/report/**/*') if path.is_file()}
    sources.update(root / path for path in ('DESCRIPTION', *GENERATORS))
    images = set(root.glob('man/figures/*.png'))
    if not images:
        raise ValueError('No gallery PNG images were found.')
    assets = images | {root / 'pkgdown/assets' / name for name in PUBLIC_REPORTS}
    output = {}
    for kind, paths in (('sources', sources), ('assets', assets)):
        missing = [str(path.relative_to(root)) for path in paths if not path.is_file()]
        if missing:
            raise ValueError('Missing required gallery files: ' + ', '.join(sorted(missing)))
        output[kind] = {path.relative_to(root).as_posix(): hashlib.sha256(path.read_bytes()).hexdigest()
                        for path in sorted(paths)}
    return output


def mismatches(expected, actual):
    differences = []
    for kind in ('sources', 'assets'):
        old, new = expected.get(kind, {}), actual[kind]
        differences.extend(f'{kind}: added {path}' for path in sorted(new.keys() - old.keys()))
        differences.extend(f'{kind}: removed {path}' for path in sorted(old.keys() - new.keys()))
        differences.extend(f'{kind}: changed {path}' for path in sorted(new.keys() & old.keys()) if new[path] != old[path])
    return differences


def browser_smoke(root, output):
    from playwright.sync_api import sync_playwright
    checks, errors = [], []

    def check(name, passed, evidence=None):
        checks.append(dict(name=name, passed=bool(passed), evidence=evidence))

    def settled(page):
        page.evaluate('()=>new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)))')

    with sync_playwright() as runtime:
        options = {'executable_path': os.environ['BROWSER_EXECUTABLE']} if os.environ.get('BROWSER_EXECUTABLE') else {}
        browser = runtime.chromium.launch(**options)
        version = browser.version
        for name in PUBLIC_REPORTS:
            for width in (1440, 390):
                page = browser.new_page(viewport={'width': width, 'height': 1000})
                page.on('pageerror', lambda error: errors.append(str(error)))
                page.route('http://**/*', lambda route: route.abort())
                page.route('https://**/*', lambda route: route.abort())
                page.goto((root / 'pkgdown/assets' / name).as_uri())
                tabs = ('overview', 'selection', 'data', 'patterns', 'evaluation', 'checks', 'provenance')
                for tab in tabs:
                    page.locator(f'[data-page-link="{tab}"]').click()
                    settled(page)
                    prefix = f'{name}/{width}/{tab}'
                    check(prefix + ': selected tab is the only visible page',
                          page.locator('.workspace-page:visible').count() == 1 and page.locator('#' + tab).is_visible())
                    overflow = page.evaluate('document.documentElement.scrollWidth>innerWidth')
                    check(prefix + ': no page overflow', not overflow)
                    if overflow:
                        page.screenshot(path=str(output / f'{name}-{width}-{tab}.png'), full_page=True)
                page.locator('[data-page-link="patterns"]').click()
                ids = page.locator('#feature-model-select option').evaluate_all('nodes=>nodes.map(node=>node.value)')
                for model in ids:
                    page.select_option('#feature-model-select', model)
                    settled(page)
                    selected = page.locator('#patterns [data-model-panel]:visible')
                    check(f'{name}/{width}/{model}: model selection changes evidence',
                          selected.count() == 1 and selected.get_attribute('data-model-panel') == model)
                page.locator('[data-page-link="evaluation"]').click()
                export = json.loads(page.locator('#axr-predictions-payload').text_content())
                panel = page.locator('[data-prediction-model]:visible')
                if export['mode'] == 'rows':
                    panel.locator('.prediction-records > summary').click()
                    link = panel.locator('[data-select-row]').first
                    check(f'{name}/{width}: exported cases are inspectable', link.count() > 0)
                    if link.count():
                        key = link.get_attribute('data-select-row')
                        link.click()
                        settled(page)
                        selected = page.evaluate('AutoXplainRData.getState().selected')
                        check(f'{name}/{width}: source link opens the exact record',
                              page.locator('#data').is_visible() and selected == key)
                else:
                    check(f'{name}/{width}: aggregate mode exposes no case links', panel.locator('[data-select-row]').count() == 0)
                page.close()
        browser.close()
    result = dict(passed=not errors and all(row['passed'] for row in checks), checks=checks,
                  browser_errors=errors, browser_version=version,
                  scope='Smoke tests of committed gallery files; detailed numerical oracles run separately.')
    (output / 'gallery-browser-checks.json').write_text(json.dumps(result, indent=2) + '\n')
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--root', type=Path, default=Path(__file__).resolve().parents[1])
    parser.add_argument('--record', action='store_true', help='Explicitly record reviewed, regenerated gallery files.')
    parser.add_argument('--browser', action='store_true', help='Smoke-test committed HTML after the hashes match.')
    parser.add_argument('--output-dir', type=Path, default=Path('/tmp/autoxplain-gallery-checks'))
    args = parser.parse_args()
    root = args.root.resolve()
    manifest = root / 'validation/gallery-manifest.json'
    try:
        actual = inventory(root)
        if args.record:
            document = dict(schema_version=1, recorded_at_utc=datetime.now(timezone.utc).isoformat(), **actual)
            manifest.parent.mkdir(parents=True, exist_ok=True)
            manifest.write_text(json.dumps(document, indent=2) + '\n')
            print(f'Recorded {len(actual["sources"])} sources and {len(actual["assets"])} gallery assets in {manifest}.')
            return 0
        if not manifest.is_file():
            raise ValueError('The committed gallery manifest is missing.')
        expected = json.loads(manifest.read_text())
        if expected.get('schema_version') != 1:
            raise ValueError('Unsupported gallery manifest schema.')
        differences = mismatches(expected, actual)
        if differences:
            raise ValueError('The committed gallery is not bound to the current source/assets:\n' + '\n'.join(differences))
        print(f'Gallery manifest matches {len(actual["sources"])} sources and {len(actual["assets"])} assets.')
        if args.browser:
            args.output_dir.mkdir(parents=True, exist_ok=True)
            result = browser_smoke(root, args.output_dir)
            print(json.dumps(dict(passed=result['passed'], checks=len(result['checks']), errors=result['browser_errors'])))
            return 0 if result['passed'] else 1
        return 0
    except (ValueError, OSError, json.JSONDecodeError) as error:
        print(str(error) + '\n' + REBUILD, file=sys.stderr)
        return 1


if __name__ == '__main__':
    raise SystemExit(main())
