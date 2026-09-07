# AutoXplainR 0.6.1 release evidence

Status: [0.6.1 is published](https://github.com/Matt17BR/autoXplainR/releases/tag/v0.6.1).
All 16 release jobs passed. The actual downloaded archive, fresh installation,
saved-result reloads and public website are verified. The authorized release
and the acceptance tasks below are complete.

## Why another revision was needed

The maintainer identified excessive empty space in the 0.6.0 gallery and a
missing Pareto frontier. The [independent inspection](audits-0.6.1/report-layout.md)
found defects in the live report too: repeated selection lists, slow access to
mobile data plots, duplicated feature labels, an unnamed uncertainty table and
almost empty final print pages. The old horizontal-overflow and gallery-hash
checks could pass all of these states.

## Acceptance tasks

- Compare observed scores and costs using a drawn frontier. Inspect equal-cost,
  equal-score, nonconvex and missing-cost cases against literal answers. A line
  must not imply unobserved intermediate models. Logarithmic cost spacing must
  be explicit and must not conceal zero measurements.
- On a phone, select a data column and reach useful distribution evidence in
  the initial view. Change raw/processed values, compare splits, inspect missing
  counts and use the keyboard to open the binning explanation.
- Compare a family's configurations with parameters, pooled scores and fold
  variation visible. Open a plotted configuration's fold record and the exact
  settings table. Preserve the recorded training-only selection arithmetic.
- Identify the model, feature and prediction target/class in a fitted curve.
  Compare models on shared axes without repeated headings obscuring the graph.
- Read regression diagnostics together and follow an exported error to its
  source record. Read uncertainty intervals with actual models, metric and units.
- Print the selected view with chart captions, axes, model summaries and method
  assumptions intact. Inspect PDF pages for isolated headings or a short leftover
  table. An extra page is acceptable when substantive evidence needs it.
- View every gallery image at its README display width. Keep complete task
  evidence in the frame; retain full-page captures separately for inspection.

These are implementation acceptance tasks and adversarial agent reviews. They
do not establish a measured usability advantage over other packages.

## Validation record

The main browser replay passed 1,367 checks across regression, binary,
multiclass and quick reports, including 28 accessibility scans. Independent
chart fixtures passed 219 checks, with separate literal frontier and cost-scale
fixtures passing 327 and 96 checks. These include ties, missing measurements,
almost equal values and nonconvex frontiers. No browser runtime errors occurred.
Counts are per runner and must not be added: the main runner invokes the chart
suite, which invokes the frontier and cost-scale suites.

Source-record and prediction checks passed 249 and 1,923 checks respectively;
the linked classification cutoff cases passed 35. Supplied-model, benchmark and
PDF checks passed 174, including screen-state restoration and retained external
citations after printing. Selection's 223-check replay verifies the actual
recorded folds, selection threshold, parameters, failed-fit links and print
restoration. After the final screen-only fold-table sizing change, we reran
the same 223-check Selection gate and the actual public reports passed all 128 layout
checks, producing all 21 selected-view PDFs. Their recorded hashes match the
committed public HTML.

The independent layout audit also ran that 128-check gate, including actual PDF
content and geometry. The same gate rejected 11 defects in the untouched 0.6.0
reports. Targeted statistical-identity tests passed 58 assertions. The audit
records rejected intermediate fixes as well as the final result. These are
task-specific constraints, not universal page-height requirements.

The data-layout check was also repaired: it previously counted a hidden tooltip
as visible overflow. It now checks actual visibility, exercises the open tooltip
with the keyboard, and rejects a deliberately clipped tooltip. Other negative
controls include excessive setup spacing, an orphaned printed chart title and
incorrect frontier paths. Deliberately displaced print values and crushed error
columns must also fail. The mutation harness now keeps all independent fixtures
available, so unrelated missing files cannot make a deliberately broken report
appear successfully detected. Both a disconnected model selector and false
chart values were rejected for their intended failures, without runtime errors.
Numerical answer data remain independent of chart coordinates and screenshot
composition.

All eight gallery images were inspected at a local review width of 896px,
alongside complete-page captures. The later actual GitHub inspection measured
approximately 838px. At the 896px review width, Selection changed from 1,132
to 540px high and Predictions from 904 to 546px. Data changed from 748 to 599px;
Checks from 779 to 508px. Comparison increased from 664 to 693px to retain the
model settings, frontier and scale controls at the more readable browser width.
The duplicate comparison embed was removed. These dimensions describe the
named examples, not an optimization target for arbitrary reports.
The final gallery browser replay passed 118 checks; its manifest binds the 65
source inputs to all three reports and eight PNGs.
An independent ordinary `render_model_report()` replay from the saved 0.6.1
fitted objects reproduced all three public HTML files byte for byte.

## Publication

Published at 2026-09-07 21:01:34 UTC from
`5e20f6723aaf3f0f5a92d09e80394e67057f1e2e`. The downloaded archive's SHA-256 is
`d5a5e9097cb1c5d9469cee02a80f020df498026548ce801cb10492d8dbb9ca14`.
It matches GitHub's asset digest and the published `SHA256SUMS`, including after
installation. Existing 0.6.0 release tags and artifacts remain unchanged.

The [portable verification record](results/release-0.6.1.json) includes source
hashes, all release-job links, actual public HTTP responses and installed-workflow
results. The [R 4.6.1 archive check](results/release-0.6.1-r-check.txt) passed
with zero errors, zero warnings and one "New submission" note, including both
manuals. The same archive's [R-devel check](results/release-0.6.1-r-devel-check.txt)
passed with zero errors, zero warnings and two notes: new submission and the
`render_model_report()` example taking 5.088 seconds. R-devel used `--no-manual`
and reported its unavailable optional packages as information; their engine and
H2O integration checks passed separately.

All 188 authored files in the published archive match the reviewed source:
187 byte hashes are identical, and DESCRIPTION matches after accounting for R's
build metadata. The complete 204-file archive inventory also matches the accepted
local build. [Installation of the actual download](results/release-0.6.1-installed-smoke.txt)
passed prediction, report, JSON export, missing-value/novel-category and
supplied-model workflows. A separate R process reloaded all 17 models across
the five scenarios and reproduced their saved outputs.

The first local archive, SHA-256
`b1238f29e2fdad91416436f9caac005491c230c7443f8dcbbb251dcc323d1d88`,
was rejected by `R CMD check --as-cran`: two new R string literals used a literal
middle dot. They now use the portable `\u00b7` escape. Parsing the old and new
files with source references disabled produced identical R expressions, and all
R source files contain only ASCII bytes. The reviewed HTML and PNG hashes did
not change; the gallery manifest records the corrected source hashes. This
failed candidate was not published.

The next local candidate, SHA-256
`698a7ab3c5329a0d72a4cd96553c292ca37e24a9234366c09ebb3d4cf6294d15`,
passed the full source check with no errors or warnings and one "New submission"
note. Its 3,934 assertions, manuals, vignettes, installed workflows and saved
11-model replay passed. It was nevertheless superseded after GitHub's wider
font exposed clipped near-tie tick labels in the no-JavaScript chart.

The independent reproduction rejected that layout at 1440, 390 and 320px.
Static charts now reserve more width for numeric labels. The clipping tolerance
is unchanged; numeric scores, costs and frontier membership are preserved.
A separate log-axis
assertion incorrectly concatenated wrapped SVG lines; it now reads the displayed
lines and requires the complete axis title. The saved-workflow inspection also
now scopes exact settings to the visible family, retaining compatibility with
historical reports and the requirement to inspect both additive configurations.

The accepted local archive, SHA-256
`3390dc078825a144079be612ccd642798b5be88b228213a4927768d561b63c9a`,
passed `R CMD check --as-cran` with zero errors, zero warnings and one
"New submission" note. All 3,934 assertions passed. Examples, rebuilt vignettes,
PDF and HTML manuals, lint and spelling passed. Regenerating documentation in
an isolated copy reproduced all 58 generated files. The archive's 188 authored
files match the accepted source; DESCRIPTION comparison permits only R's build
metadata. Installation in a fresh library and a separate R process verified
saved-result reloads across 17 models and five scenarios.

The original saved 11-model workflow was also rendered through this installed
archive without refitting or modifying its source object. The browser inspection
covered every model's settings, both additive configurations and their selection
threshold, linked source records and copied R commands. Subsequent native checks
matched copied predictions exactly and matched the rounded additive effect
values within `9.5e-6`. The dense log-cost view retained all 11 model points on
desktop and phone. One remaining presentation limit is recorded: the exact GAM
formula uses encoded predictor names, with the original-name mapping available
in the same model's "Preprocessing and encoded inputs" disclosure.

All 14 pull-request checks passed on
`aa00461eff0c71dc776a6da86832372655fc1ffd` before
[PR #4](https://github.com/Matt17BR/autoXplainR/pull/4) was merged as
`5e20f6723aaf3f0f5a92d09e80394e67057f1e2e`. The merge has the same source tree.
The annotated `v0.6.1` tag points to that merge. The subsequent
[release workflow](https://github.com/Matt17BR/autoXplainR/actions/runs/34159833212)
also passed its live H2O integration gate: 95 assertions, no failures, warnings
or skips. This is an explicit opt-in skip in the local suite. Its quality job
independently passed the 3,934-assertion suite and measured 91.94% statement
coverage; coverage alone does not establish correctness. The paid live Gemini
API test remains opt-in; no live API result
is claimed by this release's local tests.

The main-branch website deployment passed 280 public checks: 22 HTTP fetches,
all three reports at desktop and phone widths, tab navigation, settings,
selection evidence, linked source records and restored deep links. All three
HTML reports and eight PNG assets match the reviewed bytes. A separate visual
inspection of the actual GitHub README confirmed its six embedded images load,
with no duplicate Overview embed.

After release publication, the same 280-check public verification ran again and
passed at 21:04 UTC, with all 22 HTTP fetches successful and no browser errors.
All 11 public report/image assets still match the reviewed manifest. The accepted
[pkgdown build](https://github.com/Matt17BR/autoXplainR/actions/runs/34159814583)
and [Pages deployment](https://github.com/Matt17BR/autoXplainR/actions/runs/34160015063)
remain current; no new release-event website run was observed. An independent
normal render using the final tagged production source also reproduced all
three public HTML reports byte for byte without refitting.
