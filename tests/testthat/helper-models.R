make_regression_fixture <- function(n = 240L, seed = 42L) {
  set.seed(seed)
  data <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    noise = rnorm(n)
  )
  data$y <- 3 * data$x1 - 1.5 * data$x2 + rnorm(n, sd = 0.15)
  train <- data[seq_len(n * 2L / 3L), ]
  test <- data[-seq_len(n * 2L / 3L), ]
  list(
    train = train,
    test = test,
    model = lm(y ~ x1 + x2, data = train)
  )
}

make_binary_fixture <- function(n = 300L, seed = 84L) {
  set.seed(seed)
  data <- data.frame(x1 = rnorm(n), x2 = rnorm(n))
  probability <- plogis(2 * data$x1 - data$x2)
  data$y <- rbinom(n, 1, probability)
  train <- data[seq_len(200L), ]
  test <- data[-seq_len(200L), ]
  list(
    train = train,
    test = test,
    model = glm(y ~ x1 + x2, data = train, family = binomial())
  )
}

make_disjoint_evaluation <- function(data, target, rows) {
  evaluation <- data[rows, , drop = FALSE]
  predictors <- setdiff(names(data), target)
  numeric_predictors <- predictors[vapply(data[predictors], function(value) {
    is.numeric(value) && !is.logical(value)
  }, logical(1))]
  column <- if (length(numeric_predictors)) numeric_predictors[[1L]] else target
  if (!is.numeric(evaluation[[column]])) {
    stop("The test fixture needs a numeric predictor or regression outcome.", call. = FALSE)
  }
  finite <- data[[column]][is.finite(data[[column]])]
  shift <- if (length(finite)) max(abs(finite)) + 100 else 100
  missing <- is.na(evaluation[[column]])
  evaluation[[column]][!missing] <- evaluation[[column]][!missing] + shift
  rownames(evaluation) <- paste0("evaluation-", seq_len(nrow(evaluation)))
  evaluation
}
