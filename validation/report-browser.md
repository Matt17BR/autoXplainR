# Generated-report browser regression

[`check-report-browser.py`](check-report-browser.py) checks the actual HTML from
[`render-example.R`](render-example.R). The synthetic delivery example includes
comparison models, an overlapping route estimate, feature effects, diagnostics,
and uncertainty. It is independent of HTML snapshots and operates offline.

## Run the check

Use Python with `playwright==1.58.0`, its installed Chromium, `axe-core@4.13.0`,
and Poppler's `pdftotext`. R needs the package's core dependencies and `pkgload`.
The [browser workflow](../.github/workflows/report-browser.yaml) installs those
tools and runs:

```sh
Rscript validation/render-example.R
python validation/check-report-browser.py pkgdown/assets/model-report.html \
  --output-dir /tmp/autoxplain-report-browser \
  --axe-path /path/to/node_modules/axe-core/axe.min.js
```

`REPORT_PATH`, `REPORT_BROWSER_OUTPUT`, and `AXE_PATH` supply the corresponding
defaults. `--chrome-path` or `CHROME_PATH` can select a local Chromium executable;
CI uses the browser bundled with the pinned Playwright release. The JSON records
the actual browser version and input HTML SHA-256. Screenshots, PDFs and extracted
PDF text are written only to the selected output directory. The script exits
nonzero on a failed check or an execution error.

## Contracts and limits

The 51 checks cover:

- 320, 390, 768 and 1440 CSS-pixel viewports, with all disclosures closed and
  open: no page overflow; displayed quantitative SVG text at least 12 CSS pixels;
  axe WCAG 2/2.1 A/AA and 2.2 AA violations absent.
- Unique IDs, every same-document fragment target, and the expected report
  sections, target and unit label.
- Native semantic disclosures reachable by Tab with a visible outline, toggled
  by Enter and Space, with renderer JavaScript both enabled and disabled.
- Closed, open and JavaScript-disabled printed reports containing identical
  `pdftotext -layout` text after whitespace removal.
- All PDF text boxes staying within a 24-point page inset in each of those
  three print states; the report declares 12mm page margins.
- The report file remaining unchanged throughout the check.

Axe's incomplete results are recorded separately, with node counts and up to ten
example targets per rule/state. They are not counted as passing checks. The font
floor, DOM contracts and text equality cannot establish comprehension, screen
reader usability, graphical print fidelity or WCAG conformance. Firefox, Safari,
other Chromium versions, report variants and human tasks need separate evidence.

The workflow runs on pull requests and main, and exposes `workflow_call` for a
blocking release job. A local pass does not substitute for the exact-release CI
run and its uploaded artifacts.

## Executed local result

On 2026-09-07, the freshly regenerated public fixture passed all 51 checks with
no execution errors using Playwright 1.58.0, its Chromium 145.0.7632.6, and
axe-core 4.13.0. Its HTML SHA-256 was
`697c1454c314e6d0e5b219bf997a71dd71143c42943101572e25776fe289bc54`.

All ten disclosures passed the keyboard checks with JavaScript enabled and
disabled. Displayed quantitative SVG labels were at least 14 CSS pixels at every
tested width. Axe found zero A/AA violations in all eight width/disclosure
combinations; its unresolved `color-contrast` checks covered 29–50 nodes per
state and remain subject to human review.

The 13-page PDF was also visually inspected for table wrapping and page margins.
All three PDFs contained the same 15,532 non-whitespace characters and normalized
text SHA-256
`427ba1a6a0b9f06bc230031eabdf48e05f57956b432dc86d22ec8ad66f4ee8e6`.
The local evidence is `/tmp/axr-report-browser-release/report-browser.json`, with nine
screenshots and three PDFs alongside it. A portable copy of the
[JSON record](results/browser-0.4.0.json) is retained in the repository. Python compilation with warnings
treated as errors, YAML lint for the browser/release/native-engine workflows,
and `git diff --check` passed. GitHub execution against the release source is a
separate required gate.

## Deliberately broken fixture

On 2026-09-07, a copy outside the repository was altered with a duplicate ID, a
missing fragment target, 500px minimum body width, 6px SVG labels, summaries
removed from keyboard traversal, a nameless button, and print CSS that hides
closed disclosure content without JavaScript. This older fixture also lacks the
new page margins. The checker exited 1 with 38
failed checks and no execution errors. Its failures included every injected
contract category; axe reported `button-name` and `target-size` violations.

The local negative artifacts are `/tmp/axr-report-browser-negative-release/`, including
`report-browser.json`. Corrupted HTML and generated PDF/image artifacts are not
stored in the repository.
