# Reviewing the difficult regression report

This is an agent task walkthrough, not a recruited-participant study. It uses
the actual default fit on 10,000 nonlinear training rows and its independent
20,000-row evaluation set. The saved fit was rendered again after the final
report transport and presentation changes; it was not refitted for this review.

| Question | What the report made available |
| --- | --- |
| Which model was selected, and is its prediction useful? | The overview identifies the training-selected neural network: evaluation RMSE 0.7475, compared with 2.52 for the intercept-only reference. Its settings and measured costs appear beside the score and Pareto chart. |
| What was actually fitted? | Model details show six hidden units, weight decay 0.01, a 2,000-iteration limit, 20 encoded inputs, 133 weights and convergence code zero. Native-call reconstruction guidance remains available there. |
| Why these settings? | The selection tab shows all seven scheduled neural configurations, the unused preset tuples, pooled CV losses and the selection rule. The selected configuration exposes five folds with 8,000 training and 2,000 validation rows each, plus seeds and convergence status. |
| Do the explanations cover every evaluation row? | The feature tab states its 5,000-row explanation sample and distinguishes that sample from full-data scores. It retains importance values, intervals and effect-bin support. |
| Can an error be followed to its original inputs? | The prediction tab identifies 667 exported evaluation records out of 20,000. Its largest exported absolute error links to `test_data:9178`, showing the same original observation before and after preprocessing. |
| Are filtering and complete-data summaries distinguishable? | The data tab identifies the 1,000-record export from 30,000 original rows. Record filters operate on the export; the sidebar and selected charts state their respective populations. |

The linked record has observed outcome 1.8925592745005742 and prediction
-1.1067598908149268, giving residual 2.9993191653155007. Its `x3` value is missing
in the original frame and becomes -0.006728666734161594 after imputation.
These values were checked against the separately regenerated source fixture and
the saved model, rather than copied from another report representation. The
record exposes a missing important input; that observation alone does not
establish the cause of its prediction error.

The walkthrough found two presentation gaps. Native-call reconstruction prose
was repeated under every CV fold; it now appears in model details while fold
settings and diagnostics remain intact. The interaction inputs had high
permutation importance but nearly flat average effects. The existing help
tooltip now explains that averaging can hide interactions. No extra paragraph
was added to the default view, and the numerical curves were not changed.

The refreshed gallery passed 118 browser checks. Its six focused captures
retain their complete task content and have no clipped text or page overflow at
390 pixels. Only the two identical overview images changed from the previously
reviewed capture, due to measured fit times; the new image was visually checked
at GitHub README width before recording the manifest. The manifest binds 72
source files and 11 assets.

The separate layout/print run passed 128 checks across the three demonstration
reports. The overview, feature, prediction and Checks PDF pages were also
rendered to images and inspected for readable labels, complete tables and
unbroken charts. Large-record browser tasks and the WebKit parsing correction
are recorded separately in the [report scale evidence](reports/README.md).

The local review artifacts are under
`~/.cache/autoxplain-scale-0.7.0/default-report-final-review/`, with the report,
original-source answer and source hashes. The reviewed report SHA-256 is
`bca0cfa66a538d2fb606d532856a27ab4802dea221f72069dc785726740871bd`.
Gallery and print evidence are in the same cache's `gallery-capture-chunks-final`,
`gallery-browser-chunks-final` and `layout-final` directories. Final installed
archive and publication checks remain separate release requirements.
