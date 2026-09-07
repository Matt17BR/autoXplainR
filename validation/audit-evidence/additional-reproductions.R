# Run from the repository root. These print observed behavior; they are not
# passing regression tests endorsing that behavior.
pkgload::load_all(quiet = TRUE)

cat("THRESHOLD EDGE CASES\n")
print(AutoXplainR:::threshold_performance(
  rep(FALSE, 4), rep(0.1, 4), 0.5, 1, 1
)[c("sensitivity", "specificity", "balanced_accuracy")])
print(AutoXplainR:::threshold_performance(
  c(TRUE, TRUE, FALSE, FALSE), c(0.1, 0.1, 0.9, 0.9), 0.5, 1, 1
)[c("f1", "false_positives", "false_negatives")])

cat("TIED-INPUT ALE\n")
d <- data.frame(x = rep(0:4, each = 10))
d$y <- d$x
e <- explain_model(NULL, d, "y", task = "regression",
                   predict_function = function(newdata) newdata$x)
a <- explain_effect(e, feature = "x", n_points = 20)
print(as.data.frame(a)[c("x", "accumulated_effect", "n")])
print(c(effect_span = diff(range(a$accumulated_effect)),
        coordinate_span = diff(range(a$x))))

cat("BASELINE CHANGES AGGREGATE GRADE\n")
set.seed(82)
d <- data.frame(x = rnorm(240), z = rnorm(240))
d$outcome <- 3 * d$x + sin(d$z) + rnorm(240)
r <- autoxplain(d, "outcome")
ex <- as_explainers(r)
for (ids in list("main_model", c("main_model", "simple_baseline"))) {
  a <- audit_explanations(ex[ids])
  print(list(models = ids, grade = a$summary$grade,
             stable_claim_rate = a$summary$stable_claim_rate))
}

cat("NARRATIVE DOES NOT USE RETAINED EXPLANATIONS\n")
r <- autoxplain(iris, "Species", seed = 2026)
print(c(importance_rows = nrow(r$explanations$audit$importance),
        effects = length(r$explanations$effects)))
cat(generate_natural_language_report(r, provider = "local"), "\n")
