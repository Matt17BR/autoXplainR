# Release-candidate code review

Reviewed the uncommitted working tree against `292846f` on 2026-09-13.
This is a bounded source review with fresh `pkgload` counterexamples, not a
release recommendation or a predictive, resource, or browser acceptance claim.
Only this review note was written. No production code, installed packages, or
frozen candidate files were changed; no locked benchmark labels were read.

## Findings

### P2: progress handlers can change the fitted result and caller RNG

At review time, `search_progress()` in `R/search_progress.R:14` called
`message()` without preserving the RNG state. A logging handler that consumes
random numbers therefore changes the stream used by the automatically generated
folds. The baseline progress message in `R/guided_workflow.R:629` precedes the
unseeded `sample.int()` in `tuning_fold_assignment()` (`R/tuning.R:841`). Other
progress messages outside the fitting seed scope change the caller's RNG.
`preprocessing_message()` in `R/data_preprocessing.R:411` has the same handler
boundary. Importance progress already protected this case separately.

Reproduction, using only the small tree backend:

```r
pkgload::load_all(".", quiet = TRUE)
d <- data.frame(x = seq_len(80L), y = sin(seq_len(80L)))
fit <- function(verbosity) {
  set.seed(177L)
  result <- withCallingHandlers(
    autoxplain(d, "y", learners = "tree", max_models = 2L,
      nfolds = 2L, seed = 42L, verbosity = verbosity, explain = FALSE),
    message = function(condition) {
      if (startsWith(conditionMessage(condition), "AutoXplainR:")) runif(1L)
      invokeRestart("muffleMessage")
    }
  )
  list(result = result, rng = .Random.seed)
}
quiet <- fit("quiet")
info <- fit("info")
identical(quiet$result$tuning$fold_assignment, info$result$tuning$fold_assignment)
identical(quiet$rng, info$rng)
```

Both comparisons returned `FALSE`. The four CV scores changed from
`0.8231706, 0.6927285, 0.8348829, 0.7192427` to
`0.9195104, 0.8116710, 0.9363954, 0.8560103` despite the same explicit fit seed.
Existing progress parity tests used a handler that only captured and muffled
messages, so they did not cover this application-logging boundary.

### P2: total screening failure discards the actionable cause

At review time, `execute_complete_validation()` in `R/search_execution.R:123–125` reported
“Screening produced no usable configuration. Inspect the engine errors or use
`search = "grid"`.” when nothing is promoted. The screening errors were captured,
but the call returns no fitted result in which the user could inspect them.
Unlike the all-failed CV path, the terminal error omits the observed cause.

A native counterexample with 240 rows:

```r
set.seed(715)
d <- data.frame(x = runif(240, -3, 3), z = rnorm(240))
d$y <- ifelse(abs(d$x) < .3, 10, 0)
autoxplain(d, "y", task = "regression", learners = "boosting",
  max_models = 2L, nfolds = 2L, seed = 21L, verbosity = "quiet", explain = FALSE,
  tuning_control = tuning_control(search = "adaptive", metric = "rmsle",
    patience = 3L, screening_rows = 120L, threads = 1L))
```

Both enabled and disabled stopping produce the generic terminal error. The
same call with `failure_policy = "stop"` exposes the actual cause: RMSLE rejects
negative predictions and does not clip them. Rejecting those scores is correct;
the terminal all-screening-failed path should surface the captured reason.

## Correction verification

Both findings were corrected by the coordinating agent and independently
retested with fresh `pkgload` processes on 2026-09-13. No unresolved finding
remains from this bounded review.

- `search_progress()` (`R/search_progress.R:16`) and
  `preprocessing_message()` (`R/data_preprocessing.R:412`) now preserve the
  seed around message delivery. The original 80-row probe, strengthened to
  consume a random draw for every message, produced identical fold assignments,
  CV scores, OOF records, predictions, and caller RNG for quiet/info runs.
- A separate 200-row native forest probe used character conversion, missing
  value imputation, 20 trees, two folds, and 12 explanation rows. Automatic
  verbosity delivered 19 messages, including preprocessing messages. Against
  quiet mode it retained identical folds, scores, fit seeds, all 160 OOF rows,
  predictions, caller RNG, importance screening, importance objects, and effect
  curves. The small explicit grid bounded this verification; it is not a
  full-size adaptive performance measurement.
- `run_adaptive_screening()` (`R/search_execution.R:104`) now raises an
  `autoxplain_screening_error` with the observed distinct causes and structured
  `screening` and `plan` records. Both stopping variants of the original native
  RMSLE probe now expose the nonnegative-prediction requirement. Each condition
  retains both failed settings, their exact error messages, unavailable scores,
  and `screening_failed` plan statuses. It does not return a successful model.

## Additional review and evidence limits

- Inspected automatic input and thread policies, matched boosting depth
  anchors, exact retained-model settings, fold/refit stopping provenance,
  replay controls, and failure accounting. No further substantiated defect
  was found in those inspected paths. The separate forest reviewer confirmed
  the 256-tree final budget is installed before parameter keys and seeds,
  preserves full training rows, and excludes grid/custom/exact-family paths.
  Its small native parity checks and metadata-only full-size plans establish
  implementation behavior; its development diagnostics do not establish
  fitting speedup or per-class equivalence. See
  [forest-tree-budget-v3.md](forest-tree-budget-v3.md).
- An independent metric review checked AUC/RMSLE scoring, fold aggregation,
  selection and refit order, paired bootstrap, evaluation availability,
  importance, audits, and subgroup scores. One hundred randomized tied/extreme
  AUC cases matched explicit positive/negative pair comparisons. Small real
  fits confirmed maximizing AUC selection and ranking, unavailable RMSLE for
  negative evaluation predictions, and unavailable single-class AUC with
  secondary diagnostics retained.
- Reviewed test coverage for matched anchors, native prediction parity,
  skipped calibration and mixed-fold median choices, settings precision,
  and supplied/grouped replay. These establish implementation behavior, not
  competitive quality. The progress issue above was a missing meaningful
  boundary, rather than evidence that the existing parity checks were vacuous.
- README and NEWS identify these changes as development work, distinguish
  them from 0.7.0, and avoid claiming that unfinished large-data comparisons
  have passed. No full suite, large fit, browser suite, or locked acceptance
  evaluation was run by this reviewer. `git diff --check` passed.
