# Render a standalone explanation evidence report

Creates a dependency-free, accessible HTML report from an explanation
audit. The report leads with limitations and evidence grades rather than
presenting every numerical explanation as equally trustworthy.

## Usage

``` r
render_explanation_report(
  audit,
  output_file = "autoxplain-report.html",
  title = "Explanation Evidence Report",
  open = FALSE,
  ...
)
```

## Arguments

- audit:

  An `autoxplain_audit`, an `autoxplain_explainer`, or a list of
  explainers. Non-audit inputs are passed to
  [`audit_explanations()`](https://matt17br.github.io/autoXplainR/reference/audit_explanations.md).

- output_file:

  Destination `.html` path.

- title:

  Report title.

- open:

  Open the report in a browser after writing it. Defaults to `FALSE`,
  which is safe for non-interactive and CRAN environments.

- ...:

  Additional arguments passed to
  [`audit_explanations()`](https://matt17br.github.io/autoXplainR/reference/audit_explanations.md)
  when `audit` is not already an audit.

## Value

The normalized output path, invisibly.

## Examples

``` r
fit <- lm(mpg ~ wt + hp, data = mtcars)
x <- explain_model(fit, mtcars, "mpg")
audit <- audit_explanations(x, n_repeats = 3)
path <- tempfile(fileext = ".html")
render_explanation_report(audit, path)
unlink(path)
```
