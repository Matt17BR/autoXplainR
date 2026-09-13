# Undefined-score report walkthrough

Reviewed in the in-app Chromium browser on 12 September 2026, using the actual
HTML files produced by `render-metric-edge-cases.R`. These are small development
regressions, not large-data acceptance reports or a study of recruited users.

The reviewed bundle was generated at 12:51:49 UTC. Its source-bundle SHA-256 is
`479e96f142298a54a4ab557d8eeb35329edd85765a72c1f3424281aabd70fb1d`.
The renderer writes exact inputs, report hashes and the source inventory beside
the HTML. Later source changes require another rendering; this observation
does not certify every subsequent working-tree revision.

## Negative predictions with RMSLE selection

The linear model has valid training-fold RMSLE scores but makes three negative
predictions on three evaluation rows. In the opened report:

- Compare models shows the selected linear model's RMSLE as unavailable and
  the baseline's finite RMSLE as 0.5707. The primary-minus-reference card also
  says unavailable instead of implying that the model lost a valid comparison.
- Clicking **see Checks** opens the Checks tab and moves focus to its heading.
  The visible explanation identifies all three invalid predictions. It does
  not claim that clipping made those predictions acceptable.
- Changing the score selector to RMSE restores a finite comparison table and
  cost chart. The baseline scores approximately 3.609 and the linear model
  approximately 17.28. The training-selected primary is unchanged.
- Model details, prediction inspection and the remaining diagnostics stay
  accessible. Unavailable RMSLE importance and paired uncertainty retain their
  own reasons. The small-repeat warning still applies to computed baseline
  importance in this deliberately two-repeat fixture.

This inspection exposed inconsistent unavailable-score wording and an absent
navigation link in the comparison card. Both were repaired before the reviewed
bundle was generated. The model-specific reason was also shortened.

## One-class evaluation with AUC selection

Both classes occur in training; all 30 evaluation outcomes are negative. The
Checks view explains once at the model-score level that AUC requires both
classes. Its diagnostic sections separately explain unavailable AUC importance
and paired uncertainty. Finite secondary scores remain available.

An earlier rendering repeated the same model-score explanation for every
model and warned about too few shuffles even though no AUC importance could
be computed. The reviewed version aggregates the shared evaluation reason and
omits that inapplicable shuffle warning. The remaining small-evaluation and
missing-positive-class warnings describe distinct limitations of these rows.

The final release still needs the separate complete tabular workflow in
`report-acceptance-tasks.md`, including actual large fitted models, offline
navigation and cold prediction replay.
