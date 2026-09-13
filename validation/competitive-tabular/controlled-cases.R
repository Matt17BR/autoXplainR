# The same fixed generators used by validation/stress-modeling/prepare.R.
# These outcomes were inspected in the earlier release and are development data.
tabular_controlled_cases <- function() {
  set.seed(1001L)
  friedman <- as.data.frame(matrix(runif(2000L * 30L), 2000L, 30L))
  names(friedman) <- paste0("x", seq_len(ncol(friedman)))
  mu <- 10 * sin(pi * friedman$x1 * friedman$x2) +
    20 * (friedman$x3 - .5)^2 + 10 * friedman$x4 + 5 * friedman$x5
  friedman$y <- mu + rnorm(nrow(friedman))

  set.seed(1003L)
  rare <- as.data.frame(matrix(runif(4000L * 15L, -1, 1), 4000L, 15L))
  names(rare) <- paste0("x", seq_len(ncol(rare)))
  probability <- plogis(-4 + 5 * (rare$x1 > .35 & rare$x2 > .35) + 1.5 * rare$x3)
  rare$y <- factor(ifelse(runif(nrow(rare)) < probability, "yes", "no"),
    levels = c("no", "yes"))

  list(
    friedman_noise = list(data = friedman, training_rows = 1200L, task = "regression",
      original_case_sha256 = "d50cfa339fb54c2ab683dede09805879c321ff34d1addac3c919411452b273ea"),
    rare_interaction = list(data = rare, training_rows = 2500L, task = "binary",
      original_case_sha256 = "e712cf6a12b0d9651244d26d8170d4ace9152f5d55196c3de35a85cb51ac9091")
  )
}
