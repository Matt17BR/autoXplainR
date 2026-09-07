# Product walkthrough required before release

Start from a user's question, not from a list of implemented features. Use the
installed package, run the README command and open the resulting report. Work
through the tasks below before reading implementation code or the answer data.
Record the answer you could obtain, the route you took and anything that made
you hesitate. Then verify the answer against the fitted R objects. A control
that works but does not help answer the question has not passed this review.

This is an implementer walkthrough. Maintainer feedback is user feedback;
automated checks and simulated user perspectives are not participant research.

## Two complete journeys

The novice wants to predict delivery time. They know what their columns mean
but do not know RMSE, tuning or ALE. They need to know what was produced, which
model to investigate, where predictions are wrong and how to predict new rows.
Use the delivery example, then repeat with customer churn. Do not assume an
explanation that works for a numeric outcome also explains class probabilities.

The experienced user wants to challenge the analysis. They need to distinguish
training selection from held-out scores, inspect effective controls and fitted
parameters, identify dependence between inputs, check class-specific behaviour,
and reproduce the selected fit outside the report. Repeat with the three-class
flower example and an extended-portfolio fit. Include a restricted explanation
budget so that uncomputed evidence is encountered deliberately.

| Task and question | What a useful product must let the reader do | Independent R check |
|---|---|---|
| Orient: what did my command do? | Find target, task, training/evaluation counts, evaluation role and retained models without opening Methods. Distinguish fitted objects from an HTML export. | `result$models`, `result$provenance`, training/evaluation data |
| Compare: is this better than guessing? | Compare the baseline, all candidate scores and measured costs. Find score direction and units. Separate CV choice from the best held-out score; do not imply a tiny observed difference is established superiority. | Recompute loss from each explainer's predictions; inspect tuning candidates and resource measurements |
| Identify: what exactly am I comparing? | Read key settings beside the name. Open details for actual controls, formula, coefficients or fitted structure. Tell a depth cap from fitted depth and requested settings from effective values. | Native fitted object, `extract_model_characteristics(result)`, fit attributes |
| Explain: what does this model rely on? | Select a model and input, interpret signed importance, inspect the fitted curve and its support. Switch outcome class without changing the meaning of overall importance. | Permutation result, `explain_effect()` and recorded class/fingerprint |
| Challenge: could related inputs explain this? | Find a pair, retain correlation direction for numeric inputs, and read the method/sample count for mixed types. Avoid interpreting zero as independence or importance as causation. | Recompute the relevant association on the displayed training rows |
| Diagnose: where does it get things wrong? | Inspect observed-versus-predicted values or a confusion matrix, locate example mistakes and match them to evaluation rows. Distinguish a probability from a predicted label, and inspect confidence in wrong predictions. | Stored observed outcomes and actual model predictions |
| Continue: can I use and share this? | Copy the selected model's prediction command, save/reload the result, predict new rows, open the HTML offline and export the selected view with its model/class identity intact. | Execute copied code after a fresh-library install; compare predictions after save/reload |
| Recover: what is missing or invalid? | Recognize an excluded model, failed calculation or unsupported input. Find a specific next action. Never show an empty control, fabricated number or generic reassurance as a result. | Deliberately limited budgets, malformed input, missing/novel levels and retained failure records |

## Inspect, repair, repeat

1. Perform each journey at desktop size. Write answers and friction before
   consulting R or the browser-test oracle. Inspect actual plots and tables;
   a screenshot filename or successful renderer is not evidence of legibility.
2. For every visible sentence, ask which question it answers. Delete repetition.
   Put definitions in hover/focus/tap help and advanced methods in expandable
   details. Keep essential values, model identity, units and limitations of the
   displayed calculation visible where they affect interpretation.
3. Repeat the important tasks at 390px and 320px and with keyboard navigation.
   Inspect the selected-view PDF and a no-JavaScript report. Check an open model
   detail view too; testing only the page behind it misses focus and sizing bugs.
4. Verify each answer in R. A correct UI attached to the wrong model, class,
   rows or preprocessing is a release blocker. So is a vague label that prevents
   the user from identifying what was fitted.
5. Record each defect, the concrete repair and a repeat of the affected task.
   Add a regression check when the defect could recur. Preserve a negative
   control demonstrating that the interaction checks can detect a broken model
   selector. Challenge the tests too: identify the user-facing failure each one
   would catch, avoid calculating expected answers through the implementation
   under test, and try deliberate wrong outputs. Replace weak or duplicated
   assertions; do not preserve arbitrary wording or pixel sizes just to keep a
   test green. Run the broader release checks after the repairs settle.

The review is complete when both journeys yield accurate, understandable answers
without unexplained dead ends. Record unresolved limitations explicitly and
assess whether they prevent a supported task. Do not keep polishing unrelated
details indefinitely, and do not waive a failed task because the test count is
large or the page looks professional.

## Evidence for 0.5.0

The [review of the 0.4.0 regression](product-review-0.4.0.md) records observed
answers, maintainer feedback and successive repairs. Generated regression,
binary, multiclass and quick reports come from `render-explorer-cases.R`;
`check-explorer.py` checks their displayed values and interactions against R.
The final revision, test counts, screenshots and artifact checks belong in
[the release record](release-0.5.0.md). Re-run this walkthrough for later releases;
the old record is not acceptance evidence for a new UI.
