adaptive_report_fixture <- function(family, metric = "auto") {
  package <- if (family == "forest") "ranger" else "xgboost"
  skip_if_package_unavailable(package)
  set.seed(862)
  data <- data.frame(x = rnorm(260), z = rnorm(260))
  data$y <- if (metric == "auc") {
    factor(ifelse(stats::runif(260) < plogis(data$x - .5 * data$z), "yes", "no"), c("no", "yes"))
  } else {
    sin(data$x) + data$z^2 + rnorm(260, sd = 1)
  }
  autoxplain(data[1:220, ], "y", test_data = data[221:260, ],
    learners = family, max_models = 3L, nfolds = 2L, explain = FALSE, seed = 91L,
    tuning_control = tuning_control(
      search = "adaptive", metric = metric,
      screening_rows = 120L, finalists_per_family = 1L, retain_oof = FALSE
    )
  )
}

test_that("forest and CPU policy tables expose the actual resolved budgets", {
  policy <- AutoXplainR:::forest_search_policy(50000L, 90L, "regression")
  policy <- c(policy, AutoXplainR:::forest_validation_budget_policy(50000L, 90L, "adaptive"))
  cpu <- list(requested = "auto", effective = 2L, available_cores = 2L)
  html <- AutoXplainR:::selection_input_policy(list(
    task = "regression", input_policy = list(forest = policy, threads = cpu)
  ))
  expect_match(html, "128 screening; 128 per CV fit; 256 final", fixed = TRUE)
  expect_match(html, "not a convergence test", fixed = TRUE)
  expect_match(
    AutoXplainR:::selection_forest_validation_note(policy),
    "Forest CV scores use 128 trees per fit; final forests use 256", fixed = TRUE
  )
  expect_match(html, "at least four million", fixed = TRUE)
  expect_match(html, "Available CPU allocation", fixed = TRUE)
  expect_match(html, "Threads per forest or boosting fit", fixed = TRUE)
  expect_match(html, "<td>2</td>", fixed = TRUE)
  cpu$requested <- 4L
  cpu$effective <- 4L
  cpu$available_cores <- NA_integer_
  explicit <- AutoXplainR:::selection_thread_policy(cpu)
  expect_match(explicit, "Explicit thread count", fixed = TRUE)
  expect_match(explicit, "<td>4</td>", fixed = TRUE)
  expect_false(grepl("Available CPU allocation", explicit, fixed = TRUE))
})

test_that("adaptive reports retain unpromoted settings without inventing failed CV fits", {
  result <- adaptive_report_fixture("forest")
  evidence <- tuning_evidence(result)
  html <- AutoXplainR:::render_model_selection(result)
  skipped <- evidence$candidates[evidence$candidates$status == "screened_out", , drop = FALSE]
  expect_gt(nrow(skipped), 0L)
  for (index in seq_len(nrow(skipped))) {
    row <- skipped[index, , drop = FALSE]
    short <- AutoXplainR:::selection_short_parameters(row, evidence)
    expect_match(short, paste0("mtry ", row$parameters[[1L]]$mtry), fixed = TRUE)
    expect_match(short, "500 trees", fixed = TRUE)
    detail <- AutoXplainR:::selection_candidate_details(row, evidence)
    expect_match(detail, "Screened out", fixed = TRUE)
    expect_match(detail, "128", fixed = TRUE)
    expect_match(detail, "500", fixed = TRUE)
    expect_false(grepl("fold SE", detail, fixed = TRUE))
    expect_false(grepl("No validation rows were omitted", detail, fixed = TRUE))
  }
  expect_match(html, "3 screened", fixed = TRUE)
  expect_match(html, "1 advanced", fixed = TRUE)
  expect_match(html, "Screening results and compute budget", fixed = TRUE)
  expect_match(html, "Native CPU use", fixed = TRUE)
  expect_match(html, "Threads per forest or boosting fit", fixed = TRUE)
  expect_match(html, "Automatic", fixed = TRUE)
  expect_false(grepl("<strong>threads:", html, fixed = TRUE))
  expect_false(grepl("mtry ; node ;", html, fixed = TRUE))
  expect_equal(evidence$families$failed, 0L)
  chart <- AutoXplainR:::selection_candidate_plot(evidence$candidates, evidence)
  expect_match(chart, 'data-rows="1"', fixed = TRUE)
  for (id in skipped$configuration_id) expect_false(grepl(id, chart, fixed = TRUE))
  private_fields <- c("training_row", "validation_row", "validation_sampling_weight")
  expect_false(any(private_fields %in% names(evidence$screening$partition)))
})

test_that("AUC reports show the correct direction, actual rounds and replayable adaptive controls", {
  result <- adaptive_report_fixture("boosting", "auc")
  evidence <- tuning_evidence(result)
  html <- AutoXplainR:::render_model_selection(result)
  expect_match(html, "Highest CV score", fixed = TRUE)
  expect_match(html, "higher scores are better", fixed = TRUE)
  expect_match(html, "Eligible score", fixed = TRUE)
  expect_false(grepl("lower is better|Lowest CV loss|pooled CV loss", html))
  final <- evidence$candidates[evidence$candidates$final_fit, , drop = FALSE]
  short <- AutoXplainR:::selection_short_parameters(final, evidence)
  final_parameters <- evidence$refit$attempts$effective_parameters[[1L]]
  expect_match(short, paste(final_parameters$nrounds, "rounds"), fixed = TRUE)
  expect_match(short, "cap 2000", fixed = TRUE)
  code <- AutoXplainR:::selection_grid_code(result)
  env <- new.env(parent = environment())
  eval(parse(text = code), env)
  expect_identical(env$control$metric, "auc")
  expect_identical(env$control$search, "adaptive")
  expect_true(env$control$early_stopping)
  expect_identical(env$control$screening_rows, 120L)
  expect_identical(env$control$finalists_per_family, 1L)
  expect_null(env$control$grids)
  expect_null(env$control$family_budgets)
  expect_match(code, "max_models = 3L", fixed = TRUE)
  expect_match(code, "nfolds = 2L", fixed = TRUE)
  output <- tempfile(fileext = ".html")
  on.exit(unlink(output), add = TRUE)
  expect_silent(render_model_report(
    result, output, top_features = 1L, n_repeats = 1L, uncertainty = FALSE, report_data = "none"
  ))
  rendered <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(rendered, 'id="selection"', fixed = TRUE)
  expect_match(rendered, "Screening results and compute budget", fixed = TRUE)
  expect_false(grepl('cx="NA"|cx=""', rendered))
  expect_match(rendered, "Choosing the boosting rounds", fixed = TRUE)
  expect_match(rendered, "No improvement for 30 rounds", fixed = TRUE)
  expect_match(rendered, "Exact fold settings, seeds and training records", fixed = TRUE)
  expect_false(grepl("round_selection =", rendered, fixed = TRUE))
})

test_that("saved grid evidence and hostile screening notes remain usable and inert", {
  result <- autoxplain(
    mtcars, "mpg", learners = "tree", max_models = 3L,
    nfolds = 2L, explain = FALSE, tuning_control = tuning_control(search = "grid")
  )
  result$tuning$resources <- NULL
  result$tuning$control[c("search", "threads", "early_stopping", "patience")] <- NULL
  result$tuning$schema_version <- 5L
  legacy <- AutoXplainR:::render_model_selection(result)
  expect_match(legacy, "Lowest CV loss", fixed = TRUE)
  expect_false(grepl("Screening results and compute budget", legacy, fixed = TRUE))
  code <- AutoXplainR:::selection_grid_code(result)
  env <- new.env(parent = environment())
  eval(parse(text = code), env)
  expect_identical(env$control$search, "grid")
  expect_false(env$control$early_stopping)
  expect_equal(sum(env$control$family_budgets), 3L)
  evidence <- tuning_evidence(result)
  candidate <- evidence$candidates[1L, , drop = FALSE]
  score <- evidence$folds[1L, , drop = FALSE]
  score$configuration_id <- candidate$configuration_id
  score$error <- '</script><img src=x onerror="alert(1)">'
  evidence$screening <- list(
    scores = score, promotion = data.frame(configuration_id = candidate$configuration_id, reason = score$error)
  )
  html <- AutoXplainR:::selection_candidate_details(candidate, evidence)
  expect_false(grepl("<img src=x", html, fixed = TRUE))
  expect_match(html, "&lt;/script&gt;&lt;img", fixed = TRUE)
  candidate$status <- "not_validated_time_limit"
  evidence$folds <- evidence$folds[FALSE, ]
  stopped <- AutoXplainR:::selection_candidate_details(candidate, evidence)
  expect_match(stopped, "Not validated time limit", fixed = TRUE)
  expect_false(grepl("fold SE", stopped, fixed = TRUE))
  no_chart <- AutoXplainR:::selection_candidate_plot(candidate, evidence)
  expect_match(no_chart, "No setting from this family reached cross-validation", fixed = TRUE)
  expect_false(grepl("<svg", no_chart, fixed = TRUE))
})

test_that("stopping curves mark the actual chosen score and omit technical curve dumps", {
  records <- list(
    `Fold 1` = list(selected_rounds = 2L, curve = data.frame(round = 1:4, score = c(.7, .8, .78, .76))),
    `Fold 2` = list(selected_rounds = 1L, curve = data.frame(round = 1:3, score = c(.82, .8, .8)))
  )
  chart <- AutoXplainR:::selection_stopping_chart(records, "roc_auc", "test-curves")
  circles <- regmatches(chart, gregexpr("<circle[^>]+>", chart))[[1L]]
  attribute <- function(name) as.numeric(sub(paste0(".*", name, '="([^"]+)".*'), "\\1", circles))
  expect_identical(attribute("data-selected-round"), c(2, 1))
  expect_equal(attribute("data-selected-score"), c(.8, .82))
  expect_match(chart, "higher is better", fixed = TRUE)
  withr::local_options(OutDec = ",")
  expect_identical(selection_stopping_chart(records, "roc_auc", "test-curves"), chart)
  expect_identical(getOption("OutDec"), ",")
  table <- AutoXplainR:::selection_technical_rows(list(round_selection = records[[1L]], seed = 8L))
  expect_false(any(grepl("Curve", table$Setting, fixed = TRUE)))
  expect_true(any(table$Setting == "Round selection / Selected rounds" & table$Value == "2"))
})

test_that("report replay reads supplied folds from original raw rows and refuses to invent missing folds", {
  skip_if_package_unavailable("xgboost")
  set.seed(692)
  data <- data.frame(x = rnorm(150), z = rnorm(150))
  data$y <- sin(data$x) + data$z + rnorm(150, sd = .3)
  training <- data[1:120, ]
  training$x[c(7L, 68L)] <- NA_real_
  rownames(training) <- paste0("private-source-", seq_len(120L))
  evaluation <- data[121:150, ]
  ids <- rep(c("private-unit-first", "private-unit-second"), each = 60L)
  original_control <- tuning_control(
    grids = list(boosting = list(
      nrounds = 12L, eta = .1, max_depth = 2L, min_child_weight = 1,
      subsample = .8, colsample_bytree = 1, reg_alpha = 0, reg_lambda = 1
    )),
    fold_ids = ids, early_stopping = TRUE, patience = 3L
  )
  result <- autoxplain(
    training, "y", test_data = evaluation, learners = "boosting",
    max_models = 1L, nfolds = 2L, explain = FALSE, seed = 79L,
    preprocessing_config = list(missing_value_strategy = "drop_rows"), tuning_control = original_control
  )
  expect_equal(nrow(result$training_data), 118L)
  expect_equal(nrow(result$tuning$fold_assignment), 120L)
  expect_identical(result$tuning$resources$calibration_fit_attempts, 0L)
  code <- AutoXplainR:::selection_grid_code(result)
  expect_false(grepl("private-source-|private-unit-", code))
  env <- new.env(parent = environment())
  expect_error(eval(parse(text = code), env), "Set original_result", fixed = TRUE)
  env$original_result <- result
  env$original_result$tuning$fold_assignment <- result$tuning$fold_assignment[120:1, ]
  eval(parse(text = code), env)
  expect_identical(env$control$fold_ids, ids)
  replay <- autoxplain(
    training, "y", test_data = evaluation, learners = "boosting",
    max_models = NULL, nfolds = 2L, explain = FALSE, seed = 79L,
    preprocessing_config = list(missing_value_strategy = "drop_rows"), tuning_control = env$control
  )
  expect_identical(replay$tuning$fold_assignment, result$tuning$fold_assignment)
  expect_identical(replay$tuning$resources$calibration_fit_attempts, 0L)
  expect_identical(replay$tuning$fold_scores$score, result$tuning$fold_scores$score)
  expect_identical(predict(replay, evaluation), predict(result, evaluation))
  env$original_result$tuning$fold_assignment <- env$original_result$tuning$fold_assignment[-1L, ]
  expect_error(eval(parse(text = code), env), "missing or incompatible", fixed = TRUE)
  html <- AutoXplainR:::render_model_selection(result)
  expect_match(html, "original_result", fixed = TRUE)
  expect_false(grepl("private-source-|private-unit-", html))
})

test_that("grouped report replay reconstructs the group design without publishing group membership", {
  set.seed(184)
  data <- data.frame(site = rep(paste0("private-site-", 1:12), each = 10L), x = rnorm(120))
  data$y <- sin(data$x) + rnorm(120, sd = .4)
  result <- autoxplain(
    data, "y", learners = "tree", max_models = 2L, nfolds = 3L,
    validation = validation_split(group = "site"), test_fraction = .25, seed = 53L, explain = FALSE
  )
  code <- AutoXplainR:::selection_grid_code(result)
  env <- new.env(parent = environment())
  eval(parse(text = code), env)
  expect_s3_class(env$replay_validation, "autoxplain_validation")
  expect_identical(env$replay_validation$column, "site")
  expect_null(env$control$fold_ids)
  expect_false(grepl("private-site-", code, fixed = TRUE))
  expect_match(code, "test_fraction = 0.25", fixed = TRUE)
  replay <- autoxplain(
    data, "y", learners = "tree", max_models = NULL, nfolds = 3L,
    validation = env$replay_validation, test_fraction = .25, seed = 53L, explain = FALSE,
    tuning_control = env$control
  )
  expect_identical(replay$validation, result$validation)
  expect_identical(replay$tuning$fold_assignment, result$tuning$fold_assignment)
  expect_identical(replay$tuning$fold_scores$score, result$tuning$fold_scores$score)
  expect_identical(predict(replay, result$test_data), predict(result, result$test_data))
})
