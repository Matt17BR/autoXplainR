"""Require the browser checks to reject two deliberately broken reports.

Use only generated synthetic/public fixtures. Originals are never modified.
"""
import argparse
import json
from pathlib import Path
import re
import shutil
import subprocess
import sys

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--case-dir', type=Path, default=Path('/tmp/autoxplain-explorer-cases'))
parser.add_argument('--output-dir', type=Path, required=True)
parser.add_argument('--axe-path', type=Path, required=True)
args = parser.parse_args()
args.output_dir.mkdir(parents=True, exist_ok=True)
original = (args.case_dir / 'quick.html').read_text()
handler = "modelControls.forEach(control => control.addEventListener('change', () => selectModel(control.value)));"
if original.count(handler) != 1:
    raise RuntimeError('Model-selector mutation no longer matches the implementation; review it')
mutations = {'disconnected-selector': original.replace(handler, '// Deliberately disconnected.')}

# Mutate the retained source data that charts.js actually reads. The external
# R oracle and visible tables remain unchanged, so a new SVG cannot hide this.
def false_value(match):
    return match[1] + str(float(match[2]) + 7.0) + match[3]
flat, changed = re.subn(r'(<span hidden data-chart-source[^>]*? data-y=")([^"]+)(")', false_value, original)
if changed < 4:
    raise RuntimeError('Retained chart mutation changed too few points; review it')
mutations['false-graphics'] = flat
records = []
for name, html in mutations.items():
    directory = args.output_dir / name
    directory.mkdir(parents=True, exist_ok=True)
    (directory / 'quick.html').write_text(html)
    shutil.copyfile(args.case_dir / 'quick.json', directory / 'quick.json')
    # Keep unrelated chart gates intact. A missing side fixture is not evidence
    # that the deliberately broken control or numerical graphic was detected.
    for fixture in ('chart-oracle.html', 'dense-chart-oracle.html', 'dense-chart-source.json',
                    'frontier-oracle.html', 'frontier-source.json', 'cost-scale-oracle.html'):
        shutil.copyfile(args.case_dir / fixture, directory / fixture)
    with (directory / 'check.log').open('w') as log:
        run = subprocess.run([sys.executable, str(Path(__file__).with_name('check-explorer.py')),
                              '--cases', 'quick', '--case-dir', str(directory),
                              '--output-dir', str(directory / 'checks'), '--axe-path', str(args.axe_path)],
                             stdout=log, stderr=subprocess.STDOUT)
    result_path=directory/'checks/explorer-checks.json'
    if not result_path.exists():
        records.append(dict(mutation=name,caught=False,failures=[],errors=[(directory/'check.log').read_text()[-3000:]]))
        continue
    result = json.loads(result_path.read_text())
    failed = [check['name'] for check in result['checks'] if not check['passed']]
    required = (['feature panel follows model', 'selected model settings follow model']
                if name == 'disconnected-selector' else
                ['plotted costs, scores and frontier match R', 'plotted curve matches its axes and R'])
    side_checks = [check for check in result['checks']
                   if check['name'] == 'independent chart fixture geometry and no-JavaScript readability']
    side_checks_passed = len(side_checks) == 1 and side_checks[0]['passed']
    caught = run.returncode == 1 and not result['errors'] and side_checks_passed and all(
        any(expected in failure for failure in failed) for expected in required)
    records.append(dict(mutation=name, caught=caught, independent_fixtures_passed=side_checks_passed,
                        failures=failed, errors=result['errors']))
(args.output_dir / 'mutation-checks.json').write_text(json.dumps(records, indent=2))
print(json.dumps(records))
raise SystemExit(0 if all(record['caught'] for record in records) else 1)
