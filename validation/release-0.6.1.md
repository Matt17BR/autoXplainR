# AutoXplainR 0.6.1 release evidence

Status: implementation and independent acceptance are complete. Package,
archive and publication checks are in progress. No 0.6.1 release has been
published; the final archive and public verification will be recorded below.

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
fixtures passing 249 and 87 checks. These include ties, missing measurements,
almost equal values and nonconvex frontiers. No browser runtime errors occurred.

Source-record and prediction checks passed 249 and 1,923 checks respectively;
the linked classification cutoff cases passed 35. Supplied-model, benchmark and
PDF checks passed 174, including screen-state restoration and retained external
citations after printing. Selection's 223-check replay verifies the actual
recorded folds, selection threshold, parameters, failed-fit links and print
restoration. After the final screen-only fold-table sizing change, Selection
passed another 223 checks and the actual public reports passed all 128 layout
checks, producing all 21 selected-view PDFs. Their recorded hashes match the
committed public HTML.

The independent layout audit passed 128 tasks and checks, including actual PDF
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

All eight gallery images were inspected at their 896px README display width,
alongside complete-page captures. At that width, Selection changed from 1,132
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

Pending local package/archive checks, remote release gates and verification of
the actual download and website. Existing 0.6.0 release tags and artifacts remain
unchanged.

The first local archive, SHA-256
`b1238f29e2fdad91416436f9caac005491c230c7443f8dcbbb251dcc323d1d88`,
was rejected by `R CMD check --as-cran`: two new R string literals used a literal
middle dot. They now use the portable `\u00b7` escape. Parsing the old and new
files with source references disabled produced identical R expressions, and all
R source files contain only ASCII bytes. The reviewed HTML and PNG hashes did
not change; the gallery manifest records the corrected source hashes. This
failed candidate will not be published.
