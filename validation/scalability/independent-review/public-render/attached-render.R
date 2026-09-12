pkgload::load_all(".", quiet = TRUE)
out <- Sys.getenv("AXR_PUBLIC_RENDER_OUTPUT", file.path(path.expand("~/.cache"), "autoxplain-public-render-review"))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
ns <- asNamespace("AutoXplainR")
trace("model_report_html",
  tracer = quote(assign(".captured_report", result, envir = .GlobalEnv)), print = FALSE, where = ns
)
checks <- list()
for (task in c("regression", "binary", "multiclass")) {
  original <- readRDS(file.path(out, paste0(task, ".rds")))
  run <- function(name, args, check) {
    value <- tryCatch(
      {
        do.call(render_model_report, c(
          list(output_file = file.path(out, paste0(task, "-", name, ".html")), uncertainty = FALSE), args
        ))
        check(.captured_report)
        list(status = "passed")
      },
      error = function(e) list(status = "failed", reason = conditionMessage(e))
    )
    checks[[paste(task, name, sep = "-")]] <<- value
    print(list(task = task, name = name, result = value))
  }
  run("prepare",
    list(result = original, explanation_rows = 7L, n_repeats = 2L, top_features = 1L, max_models = 1L),
    function(r) stopifnot(length(r$explanations$effects) > 0L)
  )
  retained <- .captured_report
  retained$.report_context <- NULL
  retained$.report_export <- NULL
  old <- retained$explanations
  run("retained", list(result = retained, report_data = "none"), function(r) {
    stopifnot(
      identical(r$explanations$audit, old$audit), identical(r$explanations$effects_by_class, old$effects_by_class),
      identical(r$explanations$effects_by_model, old$effects_by_model)
    )
  })
  run("supplied-audit-all-curve-rows", list(
    result = retained, audit = old$audit, explanation_rows = NULL, report_data = "summary"
  ), function(r) {
    stopifnot(identical(r$explanations$audit, old$audit))
    collections <- if (task == "multiclass") unlist(
      r$explanations$effects_by_class, recursive = FALSE
    ) else r$explanations$effects_by_model
    stopifnot(length(collections) > 0L)
    for (collection in collections) {
      for (effect in collection) {
        stopifnot(
          !inherits(effect, "effect_failure"), attr(effect, "sampling")$rows_available == 19L,
          attr(effect, "sampling")$rows_used == 19L
        )
      }
    }
  })
  run("supplied-audit-and-effects", list(
    result = retained, audit = old$audit, effects = old$effects, explanation_rows = 3L,
    report_data = report_data_control("rows", max_rows = 2L, max_pair_rows = 3L, seed = 19L)
  ), function(r) {
    stopifnot(identical(r$explanations$audit, old$audit), identical(r$explanations$effects, old$effects))
  })
  # Find an explicit row export with no retained evaluation rows.
  selected_seed <- NULL
  for (seed in 1:100) {
    selected <- AutoXplainR:::data_sample_indices(c(84L, 21L), 2L, seed)[[2L]]
    if (length(selected) > 0L && all(selected %in% c(2L, 9L))) {
      selected_seed <- seed
      break
    }
  }
  stopifnot(!is.null(selected_seed))
  run("omitted-only-evaluation-sample", list(
    result = retained,
    report_data = report_data_control("rows", max_rows = 2L, max_pair_rows = 3L, seed = selected_seed)
  ), function(r) {
    html <- paste(readLines(
      file.path(out, paste0(task, "-omitted-only-evaluation-sample.html")), warn = FALSE
    ), collapse = "\n")
    stopifnot(
      grepl("No retained evaluation records are present in the exported row sample.", html, fixed = TRUE),
      r$.report_export$manifest$individual_records == 2L
    )
  })
}
untrace("model_report_html", where = ns)
jsonlite::write_json(checks, file.path(out, "attached-verdict.json"), pretty = TRUE, auto_unbox = TRUE, null = "null")
stopifnot(all(vapply(checks, function(check) identical(check$status, "passed"), logical(1))))
