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
handler = "modelControls.forEach(control => control.addEventListener('change', () => chooseModel(control.value)));"
if original.count(handler) != 1:
    raise RuntimeError('Model-selector mutation no longer matches the implementation; review it')
mutations = {'disconnected-selector': original.replace(handler, '// Deliberately disconnected.')}

def flatten(match):
    return match[1] + ' '.join(point.split(',')[0] + ',140' for point in match[2].split()) + match[3]

flat, lines = re.subn(r'(<polyline class="effect-line" points=")([^"]+)(")', flatten, original)
flat, effects = re.subn(r'(<circle class="effect-point" cx="[^"]+" cy=")[^"]+', r'\g<1>140', flat)
flat, costs = re.subn(r'<circle cx="[^"]+" cy="[^"]+" r="7" class="tradeoff-point',
                     '<circle cx="300" cy="140" r="7" class="tradeoff-point', flat)
if not all((lines, effects, costs)):
    raise RuntimeError('Plot mutation no longer changes each intended graphic; review it')
mutations['false-graphics'] = flat
records = []
for name, html in mutations.items():
    directory = args.output_dir / name
    directory.mkdir(parents=True, exist_ok=True)
    (directory / 'quick.html').write_text(html)
    shutil.copyfile(args.case_dir / 'quick.json', directory / 'quick.json')
    with (directory / 'check.log').open('w') as log:
        run = subprocess.run([sys.executable, str(Path(__file__).with_name('check-explorer.py')),
                              '--cases', 'quick', '--case-dir', str(directory),
                              '--output-dir', str(directory / 'checks'), '--axe-path', str(args.axe_path)],
                             stdout=log, stderr=subprocess.STDOUT)
    result = json.loads((directory / 'checks/explorer-checks.json').read_text())
    failed = [check['name'] for check in result['checks'] if not check['passed']]
    required = (['feature panel follows model', 'selected model settings follow model']
                if name == 'disconnected-selector' else
                ['plotted costs, scores and frontier match R', 'plotted curve matches its axes and R'])
    caught = run.returncode == 1 and not result['errors'] and all(
        any(expected in failure for failure in failed) for expected in required)
    records.append(dict(mutation=name, caught=caught, failures=failed, errors=result['errors']))
(args.output_dir / 'mutation-checks.json').write_text(json.dumps(records, indent=2))
print(json.dumps(records))
raise SystemExit(0 if all(record['caught'] for record in records) else 1)
