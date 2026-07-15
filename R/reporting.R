#' Render a standalone explanation evidence report
#'
#' Creates a dependency-free, accessible HTML report from an explanation audit.
#' The report leads with limitations and evidence grades rather than presenting
#' every numerical explanation as equally trustworthy.
#'
#' @param audit An `autoxplain_audit`, an `autoxplain_explainer`, or a list of
#'   explainers. Non-audit inputs are passed to [audit_explanations()].
#' @param output_file Destination `.html` path.
#' @param title Report title.
#' @param open Open the report in a browser after writing it. Defaults to
#'   `FALSE`, which is safe for non-interactive and CRAN environments.
#' @param ... Additional arguments passed to [audit_explanations()] when
#'   `audit` is not already an audit.
#'
#' @return The normalized output path, invisibly.
#' @export
#'
#' @examples
#' fit <- lm(mpg ~ wt + hp, data = mtcars)
#' x <- explain_model(fit, mtcars, "mpg")
#' audit <- audit_explanations(x, n_repeats = 3)
#' path <- tempfile(fileext = ".html")
#' render_explanation_report(audit, path)
#' unlink(path)
render_explanation_report <- function(audit,
                                      output_file = "autoxplain-report.html",
                                      title = "Explanation Evidence Report",
                                      open = FALSE,
                                      ...) {
  if (!inherits(audit, "autoxplain_audit")) audit <- audit_explanations(audit, ...)
  if (!is.character(output_file) || length(output_file) != 1L || is.na(output_file) ||
        !nzchar(output_file)) {
    stop("`output_file` must be a single non-empty path.", call. = FALSE)
  }
  if (!is.character(title) || length(title) != 1L || is.na(title)) {
    stop("`title` must be a single non-missing string.", call. = FALSE)
  }
  if (!is.logical(open) || length(open) != 1L || is.na(open)) {
    stop("`open` must be TRUE or FALSE.", call. = FALSE)
  }
  extension <- tolower(tools::file_ext(output_file))
  if (extension != "html") stop("`output_file` must use the .html extension.", call. = FALSE)
  directory <- dirname(output_file)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(directory)) stop("Could not create output directory: ", directory, call. = FALSE)

  html <- explanation_report_html(audit, title)
  writeLines(html, con = output_file, useBytes = TRUE)
  output_path <- normalizePath(output_file, mustWork = TRUE)
  if (open && interactive()) utils::browseURL(output_path)
  invisible(output_path)
}

explanation_report_html <- function(audit, title) {
  summary <- audit$summary
  findings <- audit$findings
  generated <- audit$provenance$created_at
  narrative <- if (!is.null(audit$optional_narrative) &&
                     length(audit$optional_narrative) == 1L &&
                     !is.na(audit$optional_narrative) &&
                     nzchar(audit$optional_narrative)) {
    paste0(
      "<section id=\"narrative\" aria-labelledby=\"narrative-title\">",
      "<p class=\"eyebrow\">Optional secondary summary</p>",
      "<h2 id=\"narrative-title\">Narrative</h2>",
      "<p>This text is subordinate to the computed evidence above. Verify it against the tables.</p>",
      "<pre class=\"narrative\">", html_escape(audit$optional_narrative), "</pre></section>"
    )
  } else {
    ""
  }
  paste0(
    "<!doctype html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
    "<title>", html_escape(title), "</title><style>", report_css(), "</style></head>",
    "<body><a class=\"skip\" href=\"#main\">Skip to report</a>",
    "<header><div class=\"shell\"><p class=\"eyebrow\">AutoXplainR / Evidence audit</p>",
    "<h1>", html_escape(title), "</h1>",
    "<p class=\"lede\">A stress test of explanation stability, feature dependence, ",
    "and near-optimal model disagreement. Generated ", html_escape(generated), ".</p>",
    "<nav aria-label=\"Report sections\"><a href=\"#findings\">Findings</a>",
    "<a href=\"#performance\">Models</a><a href=\"#importance\">Importance</a>",
    "<a href=\"#dependence\">Dependence</a><a href=\"#provenance\">Provenance</a></nav>",
    "</div></header><main id=\"main\" class=\"shell\">",
    "<section aria-labelledby=\"summary-title\"><div class=\"section-head\"><div>",
    "<p class=\"eyebrow\">Decision summary</p><h2 id=\"summary-title\">Evidence at a glance</h2>",
    "</div><span class=\"grade grade-", tolower(summary$grade), "\" aria-label=\"Evidence grade ",
    html_escape(summary$grade), "\">", html_escape(summary$grade), "</span></div>",
    "<p class=\"callout\"><strong>Scope:</strong> ", html_escape(summary$grade_note), "</p>",
    "<div class=\"cards\">",
    metric_card("Stable claims", format_percent(summary$stable_claim_rate),
                "Model-feature claims graded A or B"),
    metric_card("Near-optimal models", paste0(summary$n_near_optimal, " / ", summary$n_models),
                "Within the configured performance tolerance"),
    metric_card("Rank agreement", report_number(summary$mean_rank_agreement),
                "Mean Spearman correlation across near-optimal models"),
    metric_card("Max dependence", report_number(summary$max_association),
                "Largest pairwise feature association"),
    "</div></section>",
    "<section id=\"findings\" aria-labelledby=\"findings-title\"><p class=\"eyebrow\">Triage</p>",
    "<h2 id=\"findings-title\">Findings and actions</h2>", render_findings(findings), "</section>",
    narrative,
    "<section id=\"performance\" aria-labelledby=\"performance-title\"><p class=\"eyebrow\">Rashomon check</p>",
    "<h2 id=\"performance-title\">Supplied model set</h2>",
    "<p>Near-optimal status is relative to the supplied models and evaluation data; it is not a claim ",
    "that the full Rashomon set has been enumerated.</p>",
    html_table(audit$performance, digits = 5L), "</section>",
    "<section id=\"importance\" aria-labelledby=\"importance-title\"><p class=\"eyebrow\">Evidence, not rank alone</p>",
    "<h2 id=\"importance-title\">Permutation importance claims</h2>",
    "<p>Intervals below capture variation across random permutations. They do not provide population-level ",
    "inference. Negative importance is retained because it can reveal noise, sampling error, or model pathologies.</p>",
    render_importance(audit$importance), "</section>",
    "<section id=\"dependence\" aria-labelledby=\"dependence-title\"><p class=\"eyebrow\">Assumption pressure</p>",
    "<h2 id=\"dependence-title\">Feature dependence</h2>",
    "<p>High association warns that marginal permutations and PDPs can create unrealistic feature combinations. ",
    "Use ALE for effects and a suitable conditional importance estimator for conditional claims.</p>",
    html_table(audit$dependence, digits = 3L), "</section>",
    "<section aria-labelledby=\"method-title\"><p class=\"eyebrow\">Interpretation contract</p>",
    "<h2 id=\"method-title\">What this report can and cannot say</h2>",
    "<div class=\"columns\"><div><h3>Supported</h3><ul>",
    "<li>Descriptive model reliance on the evaluation data</li>",
    "<li>Monte Carlo stability of the implemented permutation procedure</li>",
    "<li>Observed dependence and supplied-model disagreement diagnostics</li>",
    "</ul></div><div><h3>Not established</h3><ul>",
    "<li>Causal effects or actionable interventions</li>",
    "<li>Population inference from permutation repeats</li>",
    "<li>Fairness, safety, or regulatory compliance</li>",
    "</ul></div></div></section>",
    "<section id=\"provenance\" aria-labelledby=\"provenance-title\"><p class=\"eyebrow\">Reproducibility</p>",
    "<h2 id=\"provenance-title\">Audit provenance</h2>",
    definition_list(c(
      "Generated" = audit$provenance$created_at,
      "Package version" = audit$provenance$package_version,
      "Metric" = audit$config$metric,
      "Permutation repeats" = as.character(audit$config$n_repeats),
      "Seed" = as.character(audit$config$seed),
      "Explainer IDs" = paste(audit$provenance$explainer_fingerprints, collapse = ", "),
      "Scope" = audit$provenance$diagnostic_scope
    )), "</section></main>",
    "<footer><div class=\"shell\">Generated by AutoXplainR. Preserve this report with the model, ",
    "evaluation-data version, and analysis code.</div></footer></body></html>"
  )
}

render_findings <- function(findings) {
  cards <- vapply(seq_len(nrow(findings)), function(index) {
    severity <- findings$severity[[index]]
    paste0(
      "<article class=\"finding finding-", html_escape(severity), "\">",
      "<div><span class=\"severity\">", html_escape(severity), "</span>",
      "<code>", html_escape(findings$code[[index]]), "</code></div>",
      "<h3>", html_escape(findings$message[[index]]), "</h3>",
      "<p><strong>Evidence:</strong> ", html_escape(findings$evidence[[index]]), "</p>",
      "<p><strong>Next action:</strong> ", html_escape(findings$recommendation[[index]]), "</p></article>"
    )
  }, character(1))
  paste0("<div class=\"findings\">", paste(cards, collapse = ""), "</div>")
}

render_importance <- function(importance) {
  models <- unique(importance$model)
  panels <- vapply(models, function(model) {
    item <- importance[importance$model == model, , drop = FALSE]
    item <- item[order(item$importance, decreasing = TRUE), , drop = FALSE]
    maximum <- max(abs(item$importance), na.rm = TRUE)
    if (!is.finite(maximum) || maximum == 0) maximum <- 1
    rows <- vapply(seq_len(nrow(item)), function(index) {
      width <- min(100, 100 * abs(item$importance[[index]]) / maximum)
      direction <- if (item$importance[[index]] < 0) "negative" else "positive"
      paste0(
        "<tr><th scope=\"row\">", html_escape(item$feature[[index]]), "</th>",
        "<td class=\"bar-cell\"><span class=\"bar bar-", direction,
        "\" style=\"width:", format(width, trim = TRUE, scientific = FALSE), "%\"></span></td>",
        "<td>", report_number(item$importance[[index]], 4L), "</td>",
        "<td>[", report_number(item$conf_low[[index]], 4L), ", ",
        report_number(item$conf_high[[index]], 4L), "]</td>",
        "<td><span class=\"mini-grade grade-", tolower(as.character(item$evidence_grade[[index]])),
        "\">", html_escape(as.character(item$evidence_grade[[index]])), "</span></td>",
        "<td>", html_escape(item$claim[[index]]), "</td></tr>"
      )
    }, character(1))
    paste0(
      "<article class=\"model-panel\"><h3>", html_escape(model), "</h3>",
      "<div class=\"table-wrap\"><table><thead><tr><th>Feature</th><th>Relative magnitude</th>",
      "<th>Importance</th><th>MC interval</th><th>Grade</th><th>Permitted claim</th>",
      "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div></article>"
    )
  }, character(1))
  paste(panels, collapse = "")
}

html_table <- function(data, digits = 3L) {
  headers <- paste0("<th scope=\"col\">", html_escape(names(data)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(data)), function(row) {
    cells <- vapply(data, function(column) {
      value <- column[[row]]
      if (is.numeric(value)) value <- report_number(value, digits)
      if (is.logical(value)) value <- if (isTRUE(value)) "yes" else "no"
      paste0("<td>", html_escape(as.character(value)), "</td>")
    }, character(1))
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, character(1))
  paste0("<div class=\"table-wrap\"><table><thead><tr>", headers,
         "</tr></thead><tbody>", paste(rows, collapse = ""), "</tbody></table></div>")
}

metric_card <- function(label, value, detail) {
  paste0("<article class=\"metric\"><p>", html_escape(label), "</p><strong>",
         html_escape(value), "</strong><small>", html_escape(detail), "</small></article>")
}

definition_list <- function(values) {
  entries <- paste0("<dt>", html_escape(names(values)), "</dt><dd>",
                    html_escape(unname(values)), "</dd>", collapse = "")
  paste0("<dl>", entries, "</dl>")
}

html_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

report_number <- function(x, digits = 3L) {
  if (length(x) != 1L || !is.finite(x)) return("n/a")
  format(round(x, digits), nsmall = min(2L, digits), trim = TRUE, scientific = FALSE)
}

report_css <- function() {
  paste0(
    ":root{--ink:#15231f;--muted:#5d6b66;--paper:#f6f7f2;--card:#fff;--line:#d9dfd8;",
    "--green:#176b4d;--mint:#dff3e7;--amber:#9a5b00;--amber-bg:#fff2d4;--red:#a93131;",
    "--red-bg:#fde5e2;--blue:#244d8f}*{box-sizing:border-box}html{scroll-behavior:smooth}",
    "body{margin:0;background:var(--paper);color:var(--ink);font-family:Inter,ui-sans-serif,system-ui,-apple-system,",
    "BlinkMacSystemFont,'Segoe UI',sans-serif;line-height:1.55}.shell{width:min(1120px,calc(100% - 40px));margin:auto}",
    ".skip{position:absolute;left:-9999px}.skip:focus{left:12px;top:12px;background:#fff;padding:10px;z-index:5}",
    "header{background:#102a23;color:#f8fff9;padding:64px 0 34px;border-bottom:5px solid #69c194}h1{font-size:clamp(2.2rem,5vw,4.4rem);",
    "line-height:1.02;max-width:850px;margin:8px 0 18px;letter-spacing:-.04em}.lede{max-width:760px;color:#cbe0d7;font-size:1.1rem}",
    ".eyebrow{text-transform:uppercase;letter-spacing:.14em;font-weight:800;font-size:.76rem;color:#55a97f;margin:0}",
    "nav{display:flex;flex-wrap:wrap;gap:9px;margin-top:28px}nav a{color:#effff5;text-decoration:none;border:1px solid #45675c;border-radius:999px;padding:7px 13px}",
    "main{padding:28px 0 70px}section{background:var(--card);border:1px solid var(--line);border-radius:18px;padding:clamp(22px,4vw,42px);margin:20px 0;box-shadow:0 8px 24px #173b2b0b}",
    "h2{font-size:clamp(1.55rem,3vw,2.25rem);line-height:1.15;margin:7px 0 18px;letter-spacing:-.025em}h3{line-height:1.25}.section-head{display:flex;align-items:center;justify-content:space-between;gap:20px}",
    ".grade{display:grid;place-items:center;width:82px;height:82px;border-radius:22px;font-weight:900;font-size:2.2rem}.grade-a,.mini-grade.grade-a{background:var(--mint);color:var(--green)}",
    ".grade-b,.mini-grade.grade-b{background:#e7f0fc;color:var(--blue)}.grade-c,.mini-grade.grade-c{background:var(--amber-bg);color:var(--amber)}.grade-d,.mini-grade.grade-d{background:var(--red-bg);color:var(--red)}",
    ".callout{background:#eef3ee;border-left:4px solid #628071;padding:14px 16px;border-radius:7px}.cards{display:grid;grid-template-columns:repeat(4,1fr);gap:12px;margin-top:22px}",
    ".metric{background:#f5f7f3;border:1px solid var(--line);border-radius:13px;padding:17px}.metric p{margin:0;color:var(--muted);font-weight:700;font-size:.83rem}.metric strong{display:block;font-size:1.8rem;margin:5px 0}.metric small{color:var(--muted)}",
    ".findings{display:grid;gap:12px}.finding{border:1px solid var(--line);border-left-width:5px;border-radius:12px;padding:18px}.finding h3{margin:9px 0}.finding p{margin:7px 0}.finding-critical{border-left-color:var(--red);background:#fff9f8}.finding-warning{border-left-color:#d18718;background:#fffdf6}.finding-note{border-left-color:#5283b7;background:#f9fcff}",
    ".severity{text-transform:uppercase;font-size:.72rem;font-weight:900;letter-spacing:.08em;margin-right:9px}code{background:#edf1ec;border-radius:5px;padding:2px 6px;color:#415149}.table-wrap{overflow-x:auto;border:1px solid var(--line);border-radius:11px}table{border-collapse:collapse;width:100%;font-size:.9rem;background:#fff}th,td{text-align:left;padding:11px 12px;border-bottom:1px solid #e7ebe6;vertical-align:middle}thead th{background:#edf2ed;font-size:.78rem;text-transform:uppercase;letter-spacing:.04em}tbody tr:last-child td,tbody tr:last-child th{border-bottom:0}",
    ".model-panel{margin:28px 0}.bar-cell{min-width:150px}.bar{display:block;height:9px;border-radius:999px;min-width:2px}.bar-positive{background:#2f8c65}.bar-negative{background:#c6534f}.mini-grade{display:inline-grid;place-items:center;width:30px;height:30px;border-radius:8px;font-weight:900}",
    ".columns{display:grid;grid-template-columns:1fr 1fr;gap:18px}.columns>div{background:#f5f7f3;padding:18px 22px;border-radius:12px}.columns h3{margin-top:0}.narrative{white-space:pre-wrap;overflow-wrap:anywhere;background:#f5f7f3;border:1px solid var(--line);border-radius:12px;padding:18px;font:inherit}dl{display:grid;grid-template-columns:minmax(150px,220px) 1fr;gap:8px 18px}dt{font-weight:800}dd{margin:0;color:var(--muted);overflow-wrap:anywhere}",
    "footer{background:#102a23;color:#cbe0d7;padding:28px 0}@media(max-width:820px){.cards{grid-template-columns:1fr 1fr}.columns{grid-template-columns:1fr}}@media(max-width:520px){.shell{width:min(100% - 24px,1120px)}header{padding-top:42px}.cards{grid-template-columns:1fr}.section-head{align-items:flex-start}.grade{width:62px;height:62px;font-size:1.7rem}section{border-radius:12px}dl{grid-template-columns:1fr;gap:3px}dd{margin-bottom:10px}}",
    "@media(prefers-reduced-motion:reduce){html{scroll-behavior:auto}}"
  )
}
