# Complete million-row report walkthrough

Reviewed on 12 September 2026 in the Codex Chromium browser. This is an agent
walkthrough of the actual report produced by the final one-call regression run,
not a participant study or a report regenerated from a smaller example.

Artifact: `candidate-v5/regression-1000000-controlled-full/report.html` in the
task cache. SHA-256:
`665f438dc288e3d99c02ea8916a640fc1f22d61c25c9eb8f5c3bd54a6e12b257`.
It contains 3,891,782 bytes. The separate [payload checks](final-report-payload.json)
compare its scores and populations with the original run.

| User task | Observed result |
| --- | --- |
| Identify the population and selected model | The header shows 1,000,000 training rows, 20,000 evaluation rows and three models. The comparison shows boosting RMSE 1.684 versus baseline 2.52, with rounds, depth and learning rate beside the model. |
| Understand the uncertainty | The paired interval is -0.862 to -0.813. Its help identifies fixed-model evaluation uncertainty, excludes fitting/selection uncertainty, and states the independent-observation assumption. |
| Compare performance and cost | Switching from object size to training time redraws the frontier using 6,005, 2,855 and 38 ms. All three fits are on the training-time frontier; only boosting is on the reported R-object-size frontier. These are retained-fit costs, not total search time. |
| Understand how extensively settings were searched | Model selection shows two families with one configuration each, two fold scores for boosting, no failed configurations, and explicitly says numeric sensitivity was not tested. The 1.6709 CV loss is separated from final performance. |
| Inspect the computation policy | The expandable table shows 1,000,000 training rows, 20 input/contrast columns and 20,000,000 estimated matrix cells. Its explanation says the representation was planned without outcomes or holdout rows and fixed across folds. |
| Explore data and preprocessing | Distribution summaries cover the complete training and evaluation populations. Selecting x3 shows 20,468 raw missing values across the two partitions; switching to model inputs removes that missing-value label. |
| Find individual records | The Records view states that this file contains aggregate summaries and gives `report_data = "rows"` for linked records and arbitrary filters. It does not present the explanation sample as the full dataset. |
| Interpret explanations | The page states 5,000 sampled evaluation rows out of 20,000, with full-row scores and the scope of shuffle intervals. Eight feature choices are available. Comparing regularized and boosting effects for x3 correctly shows their zero effects. |
| Inspect errors for another model | Switching predictions to the regularized model shows RMSE 2.037, MAE 1.468 and full 20,000-row diagnostics. Individual-error export limits remain visible. |
| Inspect model details at phone width | At a 390-pixel viewport override, the regularized-model dialog shows lambda 0.07412829, two nonzero coefficients, alpha 1 and path fraction 0.55. Content remains readable inside the dialog. Escape closes it and restores focus to its originating link. |

Desktop comparison and effects views, the phone-width prediction view and the
model dialog were inspected visually. At phone width, the document's client and
scroll widths were both 375 pixels after the scrollbar; only the intended tab
strip scrolls horizontally. The browser recorded no warning or error messages
during these tasks. The temporary viewport override was reset afterwards.

The fixed shallow grid is a workflow acceptance case. Its RMSE is not the
stronger model-capacity result of 0.7491, which comes from a separate, explicitly
larger grid. Completing a million-row run does not establish that these shallow
settings are adequate for every problem.

The size comparison prompted a further [measurement review](../model-size-review/README.md):
R object sizes count retained diagnostics and repeated shared labels but miss
native engine allocations. The final renderer now states this in the cost-chart
help and measurement disclosure. A separate `report-review-final.html` preserves
the original timed artifact. Its SHA-256 is
`b09ce22b6f3d739bf635f0f5c19aa3708feb05e7cad6c7d0afc8ecceca5c4f79`.
Whole-file comparison found only the two intended help replacements, with all
three JSON payloads byte-identical. Both help passages were opened in the browser
and the tooltip was inspected visually. This derivative has no new fitting or
workflow timing claim.

The final accessibility derivative, `report-accessible-final.html`, adds named
group roles to the baseline comparison and three importance containers. Its
[derivative verdict](../model-size-review/accessible-derivative-report.json)
confirms that these attributes are the only changes and all three payloads remain
byte-identical. In the actual browser, “Primary model compared with reference”
exposes 1.684, 2.52, -0.837 and the full paired interval. After selecting Feature
effects, “Feature importance for main_model” exposes all eight controls and the
sampling context of 5,000 evaluation rows out of 20,000. Prior artifacts and
timings are preserved; there was no refit or numerical change.
