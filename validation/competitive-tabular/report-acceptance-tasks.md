# Tasks for the final tabular report

Use the actual completed large-data result, exported through the public API,
in an offline browser. Record the report's byte size, export time, browser
load time and any operation that fails or stalls. Check the visible result
against the saved model and independent prediction scores. A smaller
development report cannot establish that the full-data report works.

## Choose a model and understand the cost

1. Find the selected model and explain why it was selected without reading
   implementation code. Its selection evidence must come from training
   resampling. Held-out scores can favor another retained model.
2. Compare the retained forest, booster and intercept-only baseline. Find
   the number of training and evaluation rows, the reported metric's direction,
   and the actual fitted settings. The model family name alone is insufficient.
3. Distinguish the complete search cost from one final model's training time.
   Find the explicit thread count and any configurations that failed, were
   screened out or were not attempted. Screening results must not appear as
   complete cross-validation results.
4. For an adaptive large-data forest, find its 128-tree screen, 128- or 256-tree
   full-row validation fits and 500-tree final fit. For a booster, find the
   maximum round limit, per-fold selected rounds and final fitted round count.
   These values must agree with the retained native model and tuning evidence.

## Check errors that an average score can hide

5. In Bank Marketing, find the positive class, its prevalence and its recall
   at the displayed threshold. A roughly 90% accuracy score must not obscure
   the fact that many subscribers can still be missed. Identify the excluded
   call-duration feature and the limits of random contact-row evaluation.
6. In Covertype, compare recall for aspen with the overall accuracy. Find the
   confusion counts rather than inferring class performance from one headline
   number. The report must preserve named class probabilities and the
   same-area evaluation caveat.
7. In regression, inspect the distribution of residuals, find a poor prediction
   and identify which data split it belongs to. Displayed row samples must be
   labelled as samples; scores must still describe every evaluation row.
8. If inspecting the controlled rare-event example, find an overconfident
   forest error. Its zero-probability error must remain in log loss. Do not
   improve the appearance of a result by changing metric clipping after scoring.

## Use the result outside the report

9. Save the full result, restart R and predict through the public API for every
   retained model. Compare all evaluation predictions, not a convenient first
   page, with the arrays written before saving. Check column names and factor
   class order as well as numeric values.
10. Copy the report to a separate directory and open it with network access
    disabled. Navigate its tabs, change model and feature selections, use
    keyboard-accessible explanations, and confirm that charts and tables still
    contain meaningful data. A report requiring a running R session or missing
    local files fails its shareable-report promise.

This is a reproducible product walkthrough, not a claim that a representative
sample of people has been observed using the package. Record defects and fixes
from the walkthrough before the release, alongside the numerical checks.
