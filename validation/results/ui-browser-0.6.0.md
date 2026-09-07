# Final report UI browser checks, 0.6.0

Run on 2026-09-07 against the final refreshed canonical fixtures in
`/home/mmazzarelli/.cache/autoxplain-overhaul-0.6/cases`. The refresh preserved the
original R oracle values and measured timings. Runtime: Python 3.14.4,
Playwright 1.58.0, Google Chrome 152.0.7977.82, axe-core 4.13.0.

| Gate | Result |
| --- | --- |
| Main regression, binary, multiclass and quick workflows | 1,279 / 1,279 checks; zero JavaScript errors |
| Independent hand-calculated chart fixture | 63 / 63 checks |
| Literal binary cutoff and exported-case fixture | 35 / 35 checks |
| Disconnected model selector mutation | Rejected by the three intended model state/panel assertions; zero JavaScript errors |
| Numerically false chart mutation | Rejected by cost and effect geometry assertions against the independent R oracle; zero JavaScript errors |

The chart fixture also runs as one aggregate check inside the main gate; its
63 assertions are not 63 additional main-gate assertions. Mutation failures are
expected proof that the corresponding checks detect the defect. Neither mutant
produced an unrelated failure in this run.

The main gate exercises seven tabs at 320, 390, 768 and 1440 CSS pixels; model,
metric, resource, feature, comparison and class selection; numeric chart and
importance interval geometry; model settings and dialog focus; exact affected
evidence fragment focus and scope after reload; wider system fonts; and selected
view PDF output. Print checks verify readable word bounds, evidence on the first
page, selected model/class identity and collapsed chart methodology. No-JavaScript
checks at 390 pixels verify that all sections, model specifications, chart labels,
distribution tables, ledger and export scope remain available, while inert data
controls are hidden.

The separate chart fixture uses literal values, including tiny signed effects,
unequal ALE interval widths and counts, rather than accepting retained SVG data
attributes as the expected answer. The cutoff fixture independently reconstructs
classifications, probabilities and wrong-first case order at 0, 0.50, 0.57 and 1;
it includes a first-level positive event and a model ID containing quotes. It also
checks current-cutoff captions and R code, model switches, linked row selection,
and summary/none privacy modes.

The final desktop comparison and phone feature captures were visually inspected.
No new defect was found in those views. This is implementer review, not a recruited
user study. Automated accessibility checks reported no violations in the tested
regression tabs and dialogs, but retained `color-contrast` and
`aria-prohibited-attr` incomplete results need manual review. This is not a WCAG
conformance claim. Safari, Firefox and assistive-technology user testing were not
performed in this run.

## Commands

Run from the repository root. All four commands completed with exit code zero.

```bash
export BROWSER_EXECUTABLE=/usr/bin/google-chrome
AXR_CASES=/home/mmazzarelli/.cache/autoxplain-overhaul-0.6/cases
AXR_OUTPUT=/home/mmazzarelli/.cache/autoxplain-overhaul-0.6/final-browser
AXR_PYTHON=/tmp/autoxplain-screenshot-env/bin/python
AXR_AXE=/tmp/autoxplain-audit-tools/node_modules/axe-core/axe.min.js

"$AXR_PYTHON" validation/check-explorer.py \
  --case-dir "$AXR_CASES" --output-dir "$AXR_OUTPUT/main" --axe-path "$AXR_AXE"
"$AXR_PYTHON" validation/check-report-mutations.py \
  --case-dir "$AXR_CASES" --output-dir "$AXR_OUTPUT/mutations" --axe-path "$AXR_AXE"
"$AXR_PYTHON" validation/check-chart-fixture.py \
  --case-dir "$AXR_CASES" --output-dir "$AXR_OUTPUT/charts"
"$AXR_PYTHON" validation/check-cutoff-cases.py \
  --case-dir "$AXR_CASES" --output-dir "$AXR_OUTPUT/cutoff-cases"
```

Detailed local records are `main/explorer-checks.json`,
`mutations/mutation-checks.json`, `charts/chart-fixture-checks.json` and
`cutoff-cases/cutoff-cases-checks.json` below `AXR_OUTPUT`. Screenshots and PDFs are
retained in the same directories. The main report HTML SHA256 values in this run:

| Fixture | SHA256 |
| --- | --- |
| regression | `fe1d65395ed9ae9eecd8e9e40691921834710e713a810a835f02b751907cdb88` |
| binary | `492de084fcd60d91691f5739409e6a7d9128c44b447593c69e202affb6be1356` |
| multiclass | `9a730d6a4f266fa8d5f2fefa27a2ef833d2cffaf224e5ebd34e1681126d1a7df` |
| quick | `144e20179201bf69699c9de9c763b676015846d6e6b80f7e5e16b59c9a9541ce` |
