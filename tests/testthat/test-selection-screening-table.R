screening_display_fixture <- function(metric = "rmse") {
  ids <- paste0("boosting_0", seq_len(6L))
  parameters <- lapply(seq_along(ids), function(index) {
    list(
      nrounds = 2000L, eta = .05, max_depth = 6L, min_child_weight = 1,
      subsample = .8, colsample_bytree = .8, reg_alpha = if (index == 2L) .1 else 0,
      reg_lambda = if (index == 2L) 10 else 1, encoding = "matrix"
    )
  })
  candidates <- data.frame(
    configuration_id = ids, family = "boosting", hyperparameters = "2000 trees; depth 6; eta .05",
    cv_score = c(.01, .9, .99, NA, .7, .5), cv_se = .01,
    status = c("ok", "screened_out", "screening_failed", "not_screened_time_limit", "failed", "screening_failed"),
    selected = c(TRUE, rep(FALSE, 5L)), final_fit = c(TRUE, rep(FALSE, 5L)),
    lowest_cv = c(TRUE, rep(FALSE, 5L)), within_threshold = c(TRUE, rep(FALSE, 5L)),
    retained_model_id = c("main_model", rep(NA_character_, 5L)), stringsAsFactors = FALSE
  )
  candidates$parameters <- I(parameters)
  scores <- data.frame(
    configuration_id = ids[c(1L, 2L, 3L, 5L, 6L)], score = c(.8, .6, .001, .7, NA),
    error = c("", "", "Native fit failed", "", ""), status = c("ok", "ok", "failed", "ok", "ok")
  )
  folds <- data.frame(configuration_id = character(), score = numeric(), fold = integer(),
    training_rows = integer(), validation_rows = integer(), fit_seed = integer(), error = character(),
    warning = character(), optimization_status = character()
  )
  folds$requested_parameters <- folds$effective_parameters <- folds$learned <- I(list())
  evidence <- list(
    metric = metric, task = "regression", candidates = candidates, folds = folds,
    families = data.frame(family = "boosting", best_cv = ids[[1L]]),
    screening = list(
      metric = metric, scores = scores,
      promotion = data.frame(configuration_id = ids, promoted = c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE))
    )
  )
  evidence
}

selection_test_links <- function(html) {
  links <- regmatches(html, gregexpr('<a href="#[^"]+" data-selection-inspect="[^"]+"', html))[[1L]]
  data.frame(
    href = sub('^<a href="#([^"]+)".*', "\\1", links),
    inspect = sub('.*data-selection-inspect="([^"]+)"$', "\\1", links), stringsAsFactors = FALSE
  )
}

test_that("screening ranks its own successful scores and retains every failed or missing setting", {
  evidence <- screening_display_fixture()
  ranked <- selection_screening_rows(evidence$candidates, evidence)
  expect_identical(ranked$configuration_id, paste0("boosting_0", c(2L, 5L, 1L, 3L, 4L, 6L)))
  expect_identical(ranked$status, c(
    "Not advanced", "Selected for CV", "Selected for CV", "Failed", "Not attempted", "No valid score"
  ))
  expect_identical(ranked$successful, c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE))
  # A CV failure cannot overwrite a successful screening promotion.
  expect_identical(evidence$candidates$status[[5L]], "failed")
  expect_identical(ranked$status[ranked$configuration_id == "boosting_05"], "Selected for CV")
  shuffled <- evidence
  shuffled$screening$scores <- evidence$screening$scores[c(5L, 3L, 1L, 4L, 2L), ]
  expect_identical(selection_screening_rows(evidence$candidates, shuffled), ranked)
  absent <- evidence
  absent$screening$scores <- evidence$screening$scores[FALSE, ]
  expect_identical(selection_screening_rows(evidence$candidates, absent)$status, rep("Not attempted", 6L))
  absent$screening$promotion <- NULL
  expect_identical(selection_screening_rows(evidence$candidates, absent)$status, rep("Not attempted", 6L))
  evidence$screening$promotion <- NULL
  unknown <- selection_screening_rows(evidence$candidates, evidence)
  expect_true(all(unknown$status[unknown$successful] == "Promotion not recorded"))
})

test_that("AUC screening is descending and uses the screening metric rather than CV ordering", {
  evidence <- screening_display_fixture()
  evidence$screening$metric <- "roc_auc"
  evidence$screening$scores$score[[3L]] <- .999
  ranked <- selection_screening_rows(evidence$candidates, evidence)
  expect_identical(ranked$configuration_id, paste0("boosting_0", c(1L, 5L, 2L, 3L, 4L, 6L)))
  html <- selection_screening_table(evidence$candidates, evidence)
  expect_match(html, "higher is better", fixed = TRUE)
  expect_false(grepl("lower is better", html, fixed = TRUE))
  expect_match(html, "Compare CV separately below", fixed = TRUE)
  expect_false(grepl("0.999", html, fixed = TRUE))
  expect_match(html, "Not ranked", fixed = TRUE)
  precision <- selection_test_links(selection_candidate_table(evidence$candidates, evidence))
  expect_identical(selection_test_links(html)$href,
    precision$href[match(ranked$configuration_id, evidence$candidates$configuration_id)]
  )
})

test_that("exact requested tuples distinguish penalty-only settings without expanding previews", {
  evidence <- screening_display_fixture()
  candidates <- evidence$candidates[1:2, ]
  first <- selection_requested_tuple(candidates[1L, ], evidence)
  second <- selection_requested_tuple(candidates[2L, ], evidence)
  expect_false(identical(first, second))
  expect_match(first, "reg_lambda = 1;", fixed = TRUE)
  expect_match(second, "reg_lambda = 10;", fixed = TRUE)
  expect_match(first, "reg_alpha = 0;", fixed = TRUE)
  expect_match(second, "reg_alpha = 0.1;", fixed = TRUE)
  for (name in names(candidates$parameters[[1L]])) expect_match(first, paste0(name, " = "), fixed = TRUE)
  exact <- selection_candidate_table(candidates, evidence)
  expect_match(exact, first, fixed = TRUE)
  expect_match(exact, second, fixed = TRUE)
  detail <- selection_candidate_details(candidates[2L, ], evidence)
  expect_match(detail, 'class="selection-settings"', fixed = TRUE)
  expect_match(detail, second, fixed = TRUE)
  expect_identical(selection_short_parameters(candidates[1L, ], evidence),
    selection_short_parameters(candidates[2L, ], evidence)
  )
  preview <- selection_screening_preview(candidates[2L, ], evidence)
  expect_match(preview, "lambda 10; alpha 0.1", fixed = TRUE)
  expect_false(grepl("CV|rounds|nrounds", preview))
  candidates$parameters[[1L]]$reg_lambda <- 1 + 2^-52
  full <- selection_requested_tuple(candidates[1L, ], evidence)
  value <- sub(".*reg_lambda = ([^;]+);.*", "\\1", full)
  expect_identical(as.numeric(value), 1 + 2^-52)
  expect_identical(selection_exact_parameter_value(.8), "0.8")
})

test_that("screening links reuse exact candidate targets and work as plain anchors", {
  evidence <- screening_display_fixture()
  screen <- selection_screening_table(evidence$candidates, evidence)
  precision <- selection_candidate_table(evidence$candidates, evidence)
  links <- selection_test_links(screen)
  expect_identical(links$href, links$inspect)
  expect_setequal(links$href, selection_test_links(precision)$href)
  expect_false(grepl("<details|onclick=|javascript:", screen))
  for (id in evidence$candidates$configuration_id) {
    candidate <- evidence$candidates[evidence$candidates$configuration_id == id, ]
    detail <- selection_candidate_details(candidate, evidence)
    target <- sub('^<details id="([^"]+)".*', "\\1", detail)
    expect_true(target %in% links$href)
  }
  evidence$candidates$parameters[[1L]]$reg_lambda <- '<img src=x onerror="alert(1)">'
  screen <- selection_screening_table(evidence$candidates, evidence)
  precision <- selection_candidate_table(evidence$candidates, evidence)
  expect_false(grepl("<img", paste(screen, precision), fixed = TRUE))
  expect_match(paste(screen, precision), "&lt;img", fixed = TRUE)
  evidence$screening <- NULL
  expect_identical(selection_screening_table(evidence$candidates, evidence), "")
})

test_that("adaptive family views expose screening before the separate CV chart", {
  skip_if_package_unavailable("ranger")
  data <- data.frame(x = seq_len(220L), z = sin(seq_len(220L)))
  data$y <- cos(data$x / 10) + data$z
  result <- autoxplain(
    data[1:200, ], "y", test_data = data[201:220, ], learners = "forest", max_models = 3L,
    nfolds = 2L, explain = FALSE, seed = 31L,
    tuning_control = tuning_control(search = "adaptive", screening_rows = 100L, retain_oof = FALSE)
  )
  html <- render_model_selection(result)
  screen_position <- regexpr('class="selection-screening"', html, fixed = TRUE)[[1L]]
  cv_position <- regexpr("<h4>Cross-validation</h4>", html, fixed = TRUE)[[1L]]
  chart_position <- regexpr('class="selection-chart-wrap"', html, fixed = TRUE)[[1L]]
  expect_gt(screen_position, 0L)
  expect_lt(screen_position, cv_position)
  expect_lt(cv_position, chart_position)
  expect_match(html, 'class="selection-family" data-selection-family="forest"', fixed = TRUE)
  evidence <- tuning_evidence(result)
  links <- selection_test_links(selection_screening_table(evidence$candidates, evidence))
  expect_length(links$href, 3L)
  for (target in links$href) expect_match(html, paste0('<details id="', target, '"'), fixed = TRUE)
})
test_that("technical records preserve complete seeds, named rounds and close numeric settings", {
  withr::local_options(OutDec = ",")
  record <- list(
    seed = 2147483647L,
    effective = list(reg_alpha = .10000000000000002),
    learned = list(fold_rounds = c(`Fold 1` = 218L, `Fold 2` = 395L))
  )
  rows <- selection_technical_rows(record)
  expect_identical(rows$Value[rows$Setting == "Seed"], "2147483647")
  alpha <- rows$Value[rows$Setting == "Effective / Reg alpha"]
  expect_identical(as.numeric(alpha), record$effective$reg_alpha)
  expect_false(identical(alpha, "0.1"))
  expect_identical(rows$Value[rows$Setting == "Learned / Fold rounds"], "Fold 1 = 218, Fold 2 = 395")
})
