# Reproducible task fixtures and answer data for the actual report UI.
pkgload::load_all(quiet = TRUE)
output <- Sys.getenv("EXPLORER_CASES", file.path(tempdir(), "autoxplain-explorer-cases"))
# A stable temp path lets the separate browser process locate the fixtures.
if (!nzchar(Sys.getenv("EXPLORER_CASES"))) output <- "/tmp/autoxplain-explorer-cases"
dir.create(output, recursive = TRUE, showWarnings = FALSE)
source("validation/render-example.R", local = TRUE)
regression <- result
set.seed(603)
churn <- data.frame(
  months = sample(1:60, 240, replace = TRUE),
  support_calls = rpois(240, 2),
  plan = factor(sample(c("monthly", "annual"), 240, replace = TRUE))
)
churn$left <- factor(ifelse(runif(240) < plogis(
  -.8 - .035 * churn$months + .55 * churn$support_calls - .6 * (churn$plan == "annual")
), "yes", "no"), levels = c("no", "yes"))
cases <- list(
  regression = regression,
  binary = autoxplain(churn, "left", seed = 33),
  multiclass = autoxplain(iris, "Species", seed = 33),
  quick = autoxplain(mtcars, "mpg", model_set = "quick", seed = 33)
)
for (name in names(cases)) {
  result <- cases[[name]]
  path <- file.path(output, paste0(name, ".html"))
  render_model_report(result, path, title = switch(name,
    regression = "Delivery time at dispatch",
    binary = "Customer churn",
    multiclass = "Flower species",
    quick = "Fuel consumption reference"
  ), target_units = if (name == "regression") "hours" else NULL)
  if (name %in% c("binary", "multiclass")) {
    file.copy(path, file.path("pkgdown", "assets", paste0(name, "-report.html")), overwrite = TRUE)
  }
  board <- AutoXplainR:::explorer_models(result)
  importance <- result$explanations$audit$importance
  importance <- do.call(rbind, lapply(split(importance, importance$model), function(rows) {
    head(rows[order(-rows$importance), ], result$explanations$config$top_features)
  }))
  predictions <- lapply(as_explainers(result), function(x) {
    predicted <- predict(x, x$data)
    if (result$task == "regression") {
      list(
        mean_absolute_error = mean(abs(x$y - predicted)),
        max_absolute_error = max(abs(x$y - predicted))
      )
    } else {
      labels <- if (is.matrix(predicted)) {
        colnames(predicted)[max.col(predicted, ties.method = "first")]
      } else {
        ifelse(predicted >= .5, x$class_levels[2], x$class_levels[1])
      }
      p <- if (is.matrix(predicted)) predicted[, x$class_levels, drop = FALSE] else cbind(1 - predicted, predicted)
      truth <- p[cbind(seq_along(x$y), match(x$y, x$class_levels))]
      guessed <- p[cbind(seq_along(x$y), match(labels, x$class_levels))]
      wrong <- which(labels != x$y)
      selected <- head(wrong[order(truth[wrong])], 10)
      list(mistakes = length(wrong), total = length(labels), examples = data.frame(
        row = selected, observed = as.character(x$y[selected]), predicted = labels[selected],
        predicted_probability = guessed[selected], observed_probability = truth[selected]
      ))
    }
  })
  oracle <- list(
    case = name, task = result$task, primary = result$provenance$primary_model_id,
    primary_metric = result$evaluation$primary_metric, table = board$table,
    metrics = board$metrics, resources = board$resources,
    importance = importance[c("model", "feature", "importance")], predictions = predictions,
    specifications = lapply(names(result$models), function(id) {
      spec <- AutoXplainR:::model_specification(result, id)
      list(id = id, summary = spec$summary, parameters = lapply(spec$parameters, AutoXplainR:::model_spec_value))
    }),
    classes = names(result$explanations$effects_by_class),
    class_curves = lapply(result$explanations$effects_by_class, function(models) {
      lapply(models, function(effects) {
        lapply(effects, function(effect) {
          if (inherits(effect, "effect_failure")) {
            return(NULL)
          }
          lapply(as.data.frame(effect), as.vector)
        })
      })
    }),
    curves = lapply(result$explanations$effects_by_model, function(effects) {
      lapply(effects, function(effect) {
        if (inherits(effect, "effect_failure")) {
          return(NULL)
        }
        lapply(as.data.frame(effect), as.vector)
      })
    }),
    relationships = AutoXplainR:::explorer_relationship_data(result, result$explanations$audit)$pairs
  )
  jsonlite::write_json(oracle, file.path(output, paste0(name, ".json")),
    auto_unbox = TRUE, digits = 16, pretty = TRUE, na = "null"
  )
  saveRDS(result, file.path(output, paste0(name, ".rds")))
}
cat("Generated", length(cases), "real reports and answer data in", output, "\n")
