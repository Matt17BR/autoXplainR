# Report task checks

The current report is a tabbed model explorer. Its acceptance check exercises
what a reader needs to do: compare scores and costs, switch models, choose an
input, inspect predictions and retrieve the matching R command. It compares
the displayed answers with values exported directly from R.

## Run locally

Use R with the package dependencies, `pkgload` and `jsonlite`; Python with
`playwright==1.58.0` and its Chromium; `axe-core@4.13.0`; and Poppler's
`pdftotext`.

```sh
Rscript validation/render-explorer-cases.R
python validation/check-explorer.py \
  --axe-path /path/to/node_modules/axe-core/axe.min.js \
  --output-dir /tmp/autoxplain-explorer-check
```

The fixtures cover regression, binary classification, multiclass classification
and the explicit quick mode. Their HTML, R results and answer data are written
to `/tmp/autoxplain-explorer-cases`; `EXPLORER_CASES` and `--case-dir` can change
that path. Reports use synthetic or public example data and load without network
access. Screenshots, selected-view PDFs and the JSON check record go to the
output directory. Any failed assertion or execution error exits nonzero.

## What is checked

- Every metric and resource control selects the corresponding R-computed view;
  score ordering respects whether larger or smaller values are better.
- Each model's displayed importance, prediction errors or mistake counts, and
  prediction command match that model's R result.
- Clicking an input selects its curve; selecting a relationship reports its
  association method and complete-row count.
- Tabs expose one focused page. Help works on hover, focus and tap; Escape
  dismisses it. Arrow keys navigate the tabs.
- All four examples fit within 320, 390, 768 and 1440 CSS-pixel viewports.
  Regression tabs also run axe WCAG 2/2.1 A/AA and 2.2 AA checks at every width.
- Print exports the current tab and selections. It does not dump every hidden
  model/metric combination into a PDF. Open details are included as displayed.
- Without JavaScript, all tabs and models remain available as a static document.

Axe's incomplete results are recorded separately. These checks do not establish
screen-reader usability, universal browser compatibility or participant task
completion. The accompanying [product review](product-review-0.4.0.md) records
why the old report failed and the acceptance tasks used during implementation.
The developer must also inspect the actual screenshots, controls and PDF.

The [GitHub workflow](../.github/workflows/report-browser.yaml) runs this check
on pull requests and main and is a blocking dependency of release publication.
The old `check-report-browser.py` and `results/browser-0.4.0.json` describe the
historical scrolling report; their print contracts are not the current contract.
