test_that("partial assignment retains lexical roots without capturing local state", {
  bindings <- AutoXplainR:::prediction_partial_assignment_bindings
  expect_identical(bindings(function(data) {
    state$value <- 2
    data$x + state$value
  }), "state")
  expect_identical(bindings(function(data) {
    state[["value"]] <- 2
    data$x + state$value
  }), "state")
  expect_identical(bindings(function(data) {
    state[1] <- 2
    data$x + state[[1]]
  }), "state")
  expect_identical(bindings(function(data) {
    state <- state
    state$value <- 2
    data$x
  }), "state")
  expect_identical(bindings(function(data) {
    state <- list(value = 1)
    state$value <- 2
    data$x
  }), character())
  expect_identical(bindings(function(data, state) {
    state$value <- 2
    data$x
  }), character())
  expect_identical(bindings(function(data) {
    if (length(data$x)) state <- list(value = 1)
    state$value <- 2
    data$x
  }), "state")
  expect_identical(bindings(function(data) {
    if (length(data$x)) state <- list(value = 1) else state <- list(value = 2)
    state$value <- 2
    data$x
  }), character())
  expect_identical(bindings(function(data) {
    state <- list(value = 1)
    helper <- function(x) {
      state$value <- 2
      x
    }
    helper(data$x)
  }), character())
})

test_that("partial-root analysis handles missing indices and zero-iteration loops", {
  bindings <- AutoXplainR:::prediction_partial_assignment_bindings
  expect_identical(bindings(function(data) {
    state[, "scale"] <- data[, "x", drop = FALSE]
    data[, "x"]
  }), "state")
  expect_identical(bindings(function(data) {
    for (index in integer()) state <- list(scale = 1)
    state$scale <- 2
    data$x
  }), "state")
  expect_identical(bindings(function(data) {
    for (index in seq_along(data$x)) {
      state <- list(scale = 1)
      state$scale <- 2
    }
    data$x
  }), character())
  expect_identical(bindings(function(data, state) {
    helper <- function(value) {
      state$scale <- 2
      value
    }
    helper(data$x)
  }), character())
  expect_identical(bindings(function(data) {
    unused <- quote(state$scale <- 2)
    data$x
  }), character())
})

test_that("removed bindings and nested superassignment resolve the right scope", {
  bindings <- AutoXplainR:::prediction_partial_assignment_bindings
  expect_identical(bindings(function(data) {
    state <- list(scale = 1)
    rm(state)
    state$scale <- 2
    data$x * state$scale
  }), "state")
  expect_identical(bindings(function(data) {
    state <- list(scale = 1)
    remove(list = "state")
    state[["scale"]] <- 2
    data$x
  }), "state")
  expect_identical(bindings(function(data) {
    state <- list(scale = 1)
    unused <- 1
    rm(unused)
    state$scale <- 2
    data$x
  }), character())
  expect_identical(bindings(function(data) {
    state <- list(scale = 1)
    unused <- 1
    base::rm(list = c("unused"))
    state$scale <- 2
    data$x
  }), character())
  expect_identical(bindings(function(data) {
    state <- list(scale = 1)
    base::remove(state)
    state$scale <- 2
    data$x
  }), "state")
  expect_identical(bindings(function(data) {
    state <- list(scale = 1)
    state$scale <<- 2
    data$x
  }), "state")
  expect_identical(bindings(function(data) {
    state <- list(scale = 1)
    helper <- function() {
      state$scale <<- 2
    }
    helper()
    data$x * state$scale
  }), character())
  expect_identical(bindings(function(data) {
    helper <- function() {
      state <- list(scale = 1)
      state$scale <<- 2
    }
    helper()
    data$x
  }), "state")
})

test_that("removing a local binding cannot hide later shared-state mutation", {
  state <- new.env(parent = emptyenv())
  state$scale <- 1
  data <- data.frame(x = seq(-2, 2, length.out = 24))
  data$z <- data$x^2
  data$y <- data$x
  predictor <- function(newdata) {
    state <- list(scale = 1)
    if (any(newdata$z != newdata$x^2)) {
      rm(state)
      state$scale <- 2
    }
    newdata$x + state$scale * (newdata$z - newdata$x^2)
  }
  result <- evaluate_models(list(model = list(label = "removed-local")), data, "y",
    predict_functions = list(model = predictor)
  )
  expect_identical(state$scale, 1)
  expect_error(
    AutoXplainR:::prepare_model_report_data(result, top_features = 1L, n_repeats = 1L),
    "Prediction state changed during explanation computation"
  )
  expect_identical(state$scale, 2)
  expect_identical(predictor(data), data$x)
})

test_that("removal outside the current frame has an explicit evidence boundary", {
  context <- AutoXplainR:::prediction_function_context
  callbacks <- list(
    function(data) {
      rm(state, inherits = TRUE)
      data$x
    },
    function(data) {
      remove(state, inherits = inherit)
      data$x
    },
    function(data) {
      base::rm(state, envir = state)
      data$x
    },
    function(data) {
      base::remove(state, pos = -1)
      data$x
    },
    function(data, ...) {
      rm(...)
      data$x
    },
    function(data, trigger = rm(state, inherits = TRUE)) {
      force(trigger)
      data$x
    },
    function(data) {
      helper <- function(trigger = rm(state, inherits = TRUE)) force(trigger)
      helper()
      data$x
    }
  )
  for (callback in callbacks) {
    expect_error(context(callback), "non-local or dynamic binding removal")
  }
  expect_no_error(context(function(data) {
    state <- list(scale = 1)
    rm(state, inherits = FALSE)
    data$x
  }))
  expect_no_error(context(function(data) {
    unused <- quote(rm(state, inherits = TRUE))
    data$x
  }))
  expect_no_error(context(function(data) data$rm))
  expect_error(context(function(data) {
    erase <- base::remove
    erase(state, inherits = TRUE)
    data$x
  }), "binding-removal function used as a value")
  alias <- local({
    erase <- rm
    function(data) {
      erase(state, inherits = TRUE)
      data$x
    }
  })
  expect_error(context(alias), "alias of a binding-removal function")
})

test_that("removing an enclosing binding cannot produce reusable report evidence", {
  state <- new.env(parent = emptyenv())
  state$scale <- 1
  data <- data.frame(x = seq(-2, 2, length.out = 24))
  data$z <- data$x^2
  data$y <- data$x
  predictor <- function(newdata) {
    state <- list(scale = 0)
    helper <- function() {
      if (any(newdata$z != newdata$x^2)) {
        rm(state, inherits = TRUE)
        state$scale <- 2
      }
    }
    helper()
    newdata$x + state$scale * (newdata$z - newdata$x^2)
  }
  expect_identical(predictor(data), data$x)
  expect_error({
    result <- evaluate_models(list(model = list(label = "remove-parent")), data, "y",
      predict_functions = list(model = predictor)
    )
    AutoXplainR:::prepare_model_report_data(result, top_features = 1L, n_repeats = 1L)
  }, "cannot be bound to reusable evidence.*non-local or dynamic binding removal")
  expect_identical(state$scale, 1)
})

test_that("partial assignments in lazy defaults retain outer state", {
  bindings <- AutoXplainR:::prediction_partial_assignment_bindings
  expect_identical(bindings(function(data, trigger = {
    state$scale <- 2
    NULL
  }) {
    force(trigger)
    data$x
  }), "state")
  expect_identical(bindings(function(data, trigger = {
    state <- list(scale = 1)
    state$scale <- 2
    NULL
  }) {
    force(trigger)
    data$x
  }), character())
  expect_identical(bindings(function(data) {
    helper <- function(trigger = {
      state[["scale"]] <- 2
      NULL
    }) {
      force(trigger)
    }
    helper()
    data$x
  }), "state")
  expect_identical(bindings(function(data) {
    helper <- function(trigger = {
      state <- list(scale = 1)
      state[["scale"]] <- 2
      NULL
    }) {
      force(trigger)
    }
    helper()
    data$x
  }), character())
})

test_that("default-triggered mutation cannot escape the report completion check", {
  data <- data.frame(x = seq(-2, 2, length.out = 24))
  data$z <- data$x^2
  data$y <- data$x
  for (nested in c(FALSE, TRUE)) {
    state <- new.env(parent = emptyenv())
    state$scale <- 1
    helper <- function(newdata, trigger = {
      if (any(newdata$z != newdata$x^2)) state$scale <- 2
      NULL
    }) {
      force(trigger)
      newdata$x + state$scale * (newdata$z - newdata$x^2)
    }
    predictor <- if (nested) {
      function(newdata) {
        trigger <- function(value = {
          if (any(newdata$z != newdata$x^2)) state[["scale"]] <- 2
          NULL
        }) {
          force(value)
        }
        trigger()
        newdata$x + state$scale * (newdata$z - newdata$x^2)
      }
    } else {
      function(newdata) helper(newdata)
    }
    result <- evaluate_models(list(model = list(label = "default-trigger")), data, "y",
      predict_functions = list(model = predictor)
    )
    expect_identical(state$scale, 1)
    expect_error(
      AutoXplainR:::prepare_model_report_data(result, top_features = 1L, n_repeats = 1L),
      "Prediction state changed during explanation computation"
    )
    expect_identical(predictor(data), data$x)
    expect_identical(state$scale, 2)
  }
})

test_that("partial assignment identities survive serialization and detect shared mutation", {
  data <- data.frame(x = seq(-2, 2, length.out = 24))
  data$z <- data$x^2
  data$y <- data$x
  for (access in c("dollar", "index")) {
    state <- new.env(parent = emptyenv())
    state$scale <- 1
    state$replacement <- 1
    predictor <- if (access == "dollar") {
      function(newdata) {
        state$scale <- state$replacement
        newdata$x + state$scale * (newdata$z - newdata$x^2)
      }
    } else {
      function(newdata) {
        state[["scale"]] <- state[["replacement"]]
        newdata$x + state[["scale"]] * (newdata$z - newdata$x^2)
      }
    }
    result <- evaluate_models(list(model = list(label = access)), data, "y",
      predict_functions = list(model = predictor)
    )
    context <- AutoXplainR:::prepare_report_context(result)
    fingerprint <- context$fingerprints[[1L]]
    reloaded <- unserialize(serialize(result, NULL))
    expect_identical(AutoXplainR:::prepare_report_context(reloaded)$fingerprints[[1L]], fingerprint)
    state$replacement <- 2
    expect_identical(predictor(data), data$x)
    expect_error(AutoXplainR:::prepare_report_context(result), "Stored evaluation evidence")
    expect_error(AutoXplainR:::validate_explanation_context(context), "Prediction state changed")
    expect_identical(AutoXplainR:::prepare_report_context(reloaded)$fingerprints[[1L]], fingerprint)
  }
})

test_that("locally constructed prediction state ignores unrelated outer bindings", {
  state <- new.env(parent = emptyenv())
  state$unrelated <- 1
  predictor <- function(newdata) {
    state <- list(scale = 1)
    state[["scale"]] <- 2
    newdata$x * state$scale
  }
  data <- data.frame(x = seq_len(12), y = 2 * seq_len(12))
  result <- evaluate_models(list(model = list(label = "local")), data, "y",
    predict_functions = list(model = predictor)
  )
  before <- AutoXplainR:::prepare_report_context(result)$fingerprints
  state$unrelated <- 1e9
  expect_identical(AutoXplainR:::prepare_report_context(result)$fingerprints, before)
})
