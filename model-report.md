## Prediction and evidence

RMSE was 60.4% lower than the intercept-only baseline on these test
rows.

Test independence is an assertion of this analysis. The split does not
rule out upstream leakage.

| Model                   | RMSE (hours) |
|-------------------------|--------------|
| linear regression       | 2.997        |
| Intercept-only baseline | 7.565        |

RMSE (hours) on 72 test rows

**Target:** delivery_hours (hours) · **Training rows:** 288 ·
**Evaluation rows:** 72

**Design:** reproducible random holdout. Pre-specified model; candidate
evaluation ranks did not select it. [Run details and R
commands](#provenance)

## Checks requiring attention

### \`distance_km\` exceeds the pairwise association threshold.

Affected evidence: [linear regression /
distance_km](#evidence-6d61696e5f6d6f64656c-64697374616e63655f6b6d),
[intercept-only baseline /
distance_km](#evidence-73696d706c655f626173656c696e65-64697374616e63655f6b6d),
[small decision tree /
distance_km](#evidence-736d616c6c5f74726565-64697374616e63655f6b6d),
[flexible decision tree /
distance_km](#evidence-666c657869626c655f74726565-64697374616e63655f6b6d)

Association with \`planned_route_hours\`: 0.99.

**Next:** Inspect joint support; interpret marginal shuffling as fitted
reliance and consider ALE for effects.

### \`planned_route_hours\` exceeds the pairwise association threshold.

Affected evidence: [linear regression /
planned_route_hours](#evidence-6d61696e5f6d6f64656c-706c616e6e65645f726f7574655f686f757273),
[intercept-only baseline /
planned_route_hours](#evidence-73696d706c655f626173656c696e65-706c616e6e65645f726f7574655f686f757273),
[small decision tree /
planned_route_hours](#evidence-736d616c6c5f74726565-706c616e6e65645f726f7574655f686f757273),
[flexible decision tree /
planned_route_hours](#evidence-666c657869626c655f74726565-706c616e6e65645f726f7574655f686f757273)

Association with \`distance_km\`: 0.99.

**Next:** Inspect joint support; interpret marginal shuffling as fitted
reliance and consider ALE for effects.

Diagnostic coverage and association evidence

**evaluation: computed.** Ordered observations, outcomes and event
semantics agree; independence and representativeness are not established
by these checks.

**association: computed.** Limited pairwise screen: absolute Spearman
correlation for numeric pairs, correlation ratio for mixed pairs, and
Cramer's V for categorical pairs. Small values do not establish
independence or exclude nonlinear or joint dependence.

**permutation: computed.** Loss changes describe these shuffles. The
intervals quantify shuffle randomness, not sampling, fitting or
selection uncertainty.

**comparison: insufficient evidence.** Fewer than two supplied models
meet the performance tolerance.

**resources: computed.**

**model_behavior: computed.**

**prediction_disagreement: computed.**

**decision_cutoffs: not applicable.** Decision-cutoff comparisons apply
to binary classification.

| model | features | no_observed_change | positive_loss_change | negative_loss_change | interval_includes_zero | interval_unavailable |
|----|----|----|----|----|----|----|
| main_model | 5 | 0 | 5 | 0 | 0 | 0 |
| simple_baseline | 5 | 5 | 0 | 0 | 0 | 0 |
| small_tree | 5 | 3 | 2 | 0 | 0 | 0 |
| flexible_tree | 5 | 0 | 4 | 1 | 0 | 0 |

Checks by supplied model

Separate descriptive diagnostics; no overall evidence grade. Shuffle
intervals omit evaluation-sampling, fitting and selection uncertainty.

### Feature association diagnostics

| feature | max_association | associated_feature | high_dependence | screen_status | predictors_checked |
|----|----|----|----|----|----|
| distance_km | 0.99 | planned_route_hours | yes | association_flagged | 4 |
| planned_route_hours | 0.99 | distance_km | yes | association_flagged | 4 |
| service | 0.086 | distance_km | no | limited_screen | 4 |
| dispatch_backlog | 0.083 | service | no | limited_screen | 4 |
| parcel_kg | 0.073 | dispatch_backlog | no | limited_screen | 4 |

Feature association screen

Limited pairwise screen: absolute Spearman correlation for numeric
pairs, correlation ratio for mixed pairs, and Cramer's V for categorical
pairs. Small values do not establish independence or exclude nonlinear
or joint dependence.

Independent-test check

## Prediction performance on test rows

**RMSE:** Typical prediction error, with larger mistakes weighted more
heavily; lower is better.

All evaluation metrics and definitions

The table reports every computed test-set metric. Compare models using
the metric definitions, not the rank column alone.

| Rank | Model                   | Role      | RMSE   | MAE    | R-squared |
|------|-------------------------|-----------|--------|--------|-----------|
| 1    | linear regression       | primary   | 2.9968 | 2.4813 | 0.8423    |
| 2    | flexible decision tree  | candidate | 4.1345 | 3.5088 | 0.6998    |
| 3    | small decision tree     | candidate | 4.7965 | 3.778  | 0.596     |
| 4    | intercept-only baseline | baseline  | 7.5645 | 6.3491 | -0.0048   |

Values: Rank, Model, Role

Definitions for every metric

- RMSE: Typical prediction error, with larger mistakes weighted more
  heavily; lower is better.
- MAE: Average absolute prediction error in the target's units; lower is
  better.
- R-squared: Share of evaluation-set variation explained relative to
  predicting the evaluation-set mean; higher is better.

### How large were individual errors?

Mean error

**-0.3441**Observed minus predicted; near zero means little average bias

Median absolute error

**2.5158**Half of absolute errors were below this value

90th-percentile error

**4.4514**Nine in ten absolute errors were below this value

No missing input values were observed in training or evaluation.

## How variable is this score?

Paired 95.0% percentile intervals from 1000 bootstrap draws over 72
observations. Negative differences favor the primary model.

| quantity   | estimate | lower  | upper  |
|------------|----------|--------|--------|
| primary    | 2.997    | 2.532  | 3.468  |
| baseline   | 7.565    | 6.584  | 8.436  |
| difference | -4.568   | -5.623 | -3.469 |

Values: quantity, estimate, lower

Paired percentile intervals conditional on the fitted models; negative
differences favor the primary model. These intervals omit fitting and
selection uncertainty and assume independent sampling units.

Fitted model evidence

## Patterns used for prediction

These checks describe **linear regression** on the evaluation data.
Shuffling an input measures the change in prediction loss.

| Input | Change in RMSE | Shuffle MC interval | Interpretation |
|----|----|----|----|
| distance_km | 2.303 | \[2.185, 2.421\] | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |
| planned_route_hours | 2.117 | \[1.982, 2.253\] | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |
| service | 1.987 | \[1.907, 2.067\] | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |
| dispatch_backlog | 0.406 | \[0.348, 0.464\] | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |
| parcel_kg | 0.174 | \[0.142, 0.206\] | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |

Primary-model loss changes after shuffling

Positive loss changes mean prediction worsened after shuffling.
Intervals describe shuffle Monte Carlo variation, not population
confidence. Feature association can make shuffled combinations
unrealistic.

### How predictions vary across each input

Each effect belongs to this fitted model. The plot does not predict the
consequences of intervening on the input.

### distance_km

ALE: centered change in predicted value · hours

Centered fitted effects for predicted value range from -6.368 to 5.598.
Read changes against the input values and support below.

![](data:image/svg+xml;base64,PHN2ZyBjbGFzcz0iZWZmZWN0LXBsb3QiIHN0eWxlPSJtaW4td2lkdGg6NjAwcHgiIHZpZXdib3g9IjAgMCA2MDAgMzUwIiByb2xlPSJpbWciIGFyaWEtbGFiZWw9IkFMRSBmb3IgZGlzdGFuY2Vfa20gOiBpbnB1dCB2YWx1ZXMsIGZpdHRlZCBlZmZlY3QsIHplcm8gcmVmZXJlbmNlIGZvciBBTEUsIGFuZCByZWxhdGl2ZSBzdXBwb3J0LiBGdWxsIHZhbHVlcyBpbiB0aGUgZm9sbG93aW5nIHRhYmxlLiI+PHRleHQgeD0iNzAiIHk9IjE4IiBjbGFzcz0iYXhpcy1sYWJlbCI+Q2VudGVyZWQgZml0dGVkIGVmZmVjdDwvdGV4dD48cG9seWdvbiBjbGFzcz0iZWZmZWN0LWJhbmQiIHBvaW50cz0iNzAsMjM1IDk4LjA2MzAzODk1MzMxNTUsMjIzLjUzOTk5NDA1MjkyOSAxMzMuMTQxODM3NjQ0OTYsMjA5LjIxNDk4NjYxOTA5IDE1Mi40NzI0OTQ3OTYzMTMsMjAxLjMyMDk5MzE2MDg2OCAxNzguNjY5NjQwMjAyMiwxOTAuNjIyOTU1Njk0MzIxIDI0NC43MjIyNzE3ODExNDgsMTYzLjY0OTI3MTQ4Mzc5NCAyNjkuMjAyNzk1MTIzNDAyLDE1My42NTIyNDUwMTkzMjggMzQyLjQ5NTA5MzY2NjM2OSwxMjMuNzIyMTIzMTA0MzcxIDM1OC45ODk1OTI2MjU2MzIsMTE2Ljk4NjMyMTczNjU0NSAzOTguOTk0MzUwMjgyNDg2LDEwMC42NDk3MTc1MTQxMjQgNDI5Ljg5MzU0NzQyNzg5Miw4OC4wMzE1MTk0NzY2NTc5IDQ1Mi4zNTg5MDU3Mzg5MjQsNzguODU3NDE4OTcxMTU2OCA0NTguMTgwNDkzNjA2ODk5LDc2LjQ4MDA3NzMxMTkyMzkgNDg5LjQ1Mjg2OTQ2MTc5LDYzLjcwOTQ4NTU3ODM1MjggNTA3LjczODYyNjIyNjU4Myw1Ni4yNDIxOTQ0NjkyMjQgNTM1Ljg3NjMwMDkyMTc5Niw0NC43NTE3MDk3ODI5MzIxIDU3MiwzMC4wMDAwMDAwMDAwMDAyIDU3MiwzMCA1MzUuODc2MzAwOTIxNzk2LDQ0Ljc1MTcwOTc4MjkzMTkgNTA3LjczODYyNjIyNjU4Myw1Ni4yNDIxOTQ0NjkyMjM5IDQ4OS40NTI4Njk0NjE3OSw2My43MDk0ODU1NzgzNTI3IDQ1OC4xODA0OTM2MDY4OTksNzYuNDgwMDc3MzExOTIzOCA0NTIuMzU4OTA1NzM4OTI0LDc4Ljg1NzQxODk3MTE1NjcgNDI5Ljg5MzU0NzQyNzg5Miw4OC4wMzE1MTk0NzY2NTc3IDM5OC45OTQzNTAyODI0ODYsMTAwLjY0OTcxNzUxNDEyNCAzNTguOTg5NTkyNjI1NjMyLDExNi45ODYzMjE3MzY1NDUgMzQyLjQ5NTA5MzY2NjM2OSwxMjMuNzIyMTIzMTA0MzcxIDI2OS4yMDI3OTUxMjM0MDIsMTUzLjY1MjI0NTAxOTMyOCAyNDQuNzIyMjcxNzgxMTQ4LDE2My42NDkyNzE0ODM3OTQgMTc4LjY2OTY0MDIwMjIsMTkwLjYyMjk1NTY5NDMyIDE1Mi40NzI0OTQ3OTYzMTMsMjAxLjMyMDk5MzE2MDg2OCAxMzMuMTQxODM3NjQ0OTYsMjA5LjIxNDk4NjYxOTA5IDk4LjA2MzAzODk1MzMxNTUsMjIzLjUzOTk5NDA1MjkyOSA3MCwyMzUiPjwvcG9seWdvbj48dGV4dCB4PSIyMTAuOTg2OTE2NDQzNjUxIiB5PSIyNTciIGNsYXNzPSJ0aWNrIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIj4yMDAuMDA8L3RleHQ+PHRleHQgeD0iMzYwLjI1ODQwMDIzNzg4MyIgeT0iMjU3IiBjbGFzcz0idGljayIgdGV4dC1hbmNob3I9Im1pZGRsZSI+NDAwLjAwPC90ZXh0Pjx0ZXh0IHg9IjUwOS41Mjk4ODQwMzIxMTQiIHk9IjI1NyIgY2xhc3M9InRpY2siIHRleHQtYW5jaG9yPSJtaWRkbGUiPjYwMC4wMDwvdGV4dD48bGluZSBjbGFzcz0iZ3JpZC1saW5lIiB4MT0iNzAiIHgyPSI1NzIiIHkxPSIyMTEuNTU3MTQwMDMxMTM5IiB5Mj0iMjExLjU1NzE0MDAzMTEzOSI+PC9saW5lPjx0ZXh0IHg9IjYwIiB5PSIyMTYuNTU3MTQwMDMxMTM5IiB0ZXh0LWFuY2hvcj0iZW5kIiBjbGFzcz0idGljayI+LTUuMDA8L3RleHQ+PGxpbmUgY2xhc3M9ImdyaWQtbGluZSIgeDE9IjcwIiB4Mj0iNTcyIiB5MT0iMTI1Ljg5Nzk2NjQzMjA4OCIgeTI9IjEyNS44OTc5NjY0MzIwODgiPjwvbGluZT48dGV4dCB4PSI2MCIgeT0iMTMwLjg5Nzk2NjQzMjA4OCIgdGV4dC1hbmNob3I9ImVuZCIgY2xhc3M9InRpY2siPjAuMDA8L3RleHQ+PGxpbmUgY2xhc3M9ImdyaWQtbGluZSIgeDE9IjcwIiB4Mj0iNTcyIiB5MT0iNDAuMjM4NzkyODMzMDM2MSIgeTI9IjQwLjIzODc5MjgzMzAzNjEiPjwvbGluZT48dGV4dCB4PSI2MCIgeT0iNDUuMjM4NzkyODMzMDM2MSIgdGV4dC1hbmNob3I9ImVuZCIgY2xhc3M9InRpY2siPjUuMDA8L3RleHQ+PGxpbmUgY2xhc3M9Inplcm8tbGluZSIgeDE9IjcwIiB4Mj0iNTcyIiB5MT0iMTI1Ljg5Nzk2NjQzMjA4OCIgeTI9IjEyNS44OTc5NjY0MzIwODgiPjwvbGluZT48dGV4dCB4PSI1NzIiIHk9IjExOS44OTc5NjY0MzIwODgiIGNsYXNzPSJ0aWNrIiB0ZXh0LWFuY2hvcj0iZW5kIj56ZXJvPC90ZXh0Pjxwb2x5bGluZSBjbGFzcz0iZWZmZWN0LWxpbmUiIHBvaW50cz0iNzAsMjM1IDk4LjA2MzAzODk1MzMxNTUsMjIzLjUzOTk5NDA1MjkyOSAxMzMuMTQxODM3NjQ0OTYsMjA5LjIxNDk4NjYxOTA5IDE1Mi40NzI0OTQ3OTYzMTMsMjAxLjMyMDk5MzE2MDg2OCAxNzguNjY5NjQwMjAyMiwxOTAuNjIyOTU1Njk0MzIgMjQ0LjcyMjI3MTc4MTE0OCwxNjMuNjQ5MjcxNDgzNzk0IDI2OS4yMDI3OTUxMjM0MDIsMTUzLjY1MjI0NTAxOTMyOCAzNDIuNDk1MDkzNjY2MzY5LDEyMy43MjIxMjMxMDQzNzEgMzU4Ljk4OTU5MjYyNTYzMiwxMTYuOTg2MzIxNzM2NTQ1IDM5OC45OTQzNTAyODI0ODYsMTAwLjY0OTcxNzUxNDEyNCA0MjkuODkzNTQ3NDI3ODkyLDg4LjAzMTUxOTQ3NjY1NzggNDUyLjM1ODkwNTczODkyNCw3OC44NTc0MTg5NzExNTY3IDQ1OC4xODA0OTM2MDY4OTksNzYuNDgwMDc3MzExOTIzOCA0ODkuNDUyODY5NDYxNzksNjMuNzA5NDg1NTc4MzUyNyA1MDcuNzM4NjI2MjI2NTgzLDU2LjI0MjE5NDQ2OTIyNCA1MzUuODc2MzAwOTIxNzk2LDQ0Ljc1MTcwOTc4MjkzMiA1NzIsMzAuMDAwMDAwMDAwMDAwMSI+PC9wb2x5bGluZT48Y2lyY2xlIGNsYXNzPSJlZmZlY3QtcG9pbnQiIGN4PSI3MCIgY3k9IjIzNSIgcj0iMy41Ij48L2NpcmNsZT48Y2lyY2xlIGNsYXNzPSJlZmZlY3QtcG9pbnQiIGN4PSI5OC4wNjMwMzg5NTMzMTU1IiBjeT0iMjIzLjUzOTk5NDA1MjkyOSIgcj0iMy41Ij48L2NpcmNsZT48Y2lyY2xlIGNsYXNzPSJlZmZlY3QtcG9pbnQiIGN4PSIxMzMuMTQxODM3NjQ0OTYiIGN5PSIyMDkuMjE0OTg2NjE5MDkiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iMTUyLjQ3MjQ5NDc5NjMxMyIgY3k9IjIwMS4zMjA5OTMxNjA4NjgiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iMTc4LjY2OTY0MDIwMjIiIGN5PSIxOTAuNjIyOTU1Njk0MzIiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iMjQ0LjcyMjI3MTc4MTE0OCIgY3k9IjE2My42NDkyNzE0ODM3OTQiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iMjY5LjIwMjc5NTEyMzQwMiIgY3k9IjE1My42NTIyNDUwMTkzMjgiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iMzQyLjQ5NTA5MzY2NjM2OSIgY3k9IjEyMy43MjIxMjMxMDQzNzEiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iMzU4Ljk4OTU5MjYyNTYzMiIgY3k9IjExNi45ODYzMjE3MzY1NDUiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iMzk4Ljk5NDM1MDI4MjQ4NiIgY3k9IjEwMC42NDk3MTc1MTQxMjQiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iNDI5Ljg5MzU0NzQyNzg5MiIgY3k9Ijg4LjAzMTUxOTQ3NjY1NzgiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iNDUyLjM1ODkwNTczODkyNCIgY3k9Ijc4Ljg1NzQxODk3MTE1NjciIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iNDU4LjE4MDQ5MzYwNjg5OSIgY3k9Ijc2LjQ4MDA3NzMxMTkyMzgiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iNDg5LjQ1Mjg2OTQ2MTc5IiBjeT0iNjMuNzA5NDg1NTc4MzUyNyIgcj0iMy41Ij48L2NpcmNsZT48Y2lyY2xlIGNsYXNzPSJlZmZlY3QtcG9pbnQiIGN4PSI1MDcuNzM4NjI2MjI2NTgzIiBjeT0iNTYuMjQyMTk0NDY5MjI0IiByPSIzLjUiPjwvY2lyY2xlPjxjaXJjbGUgY2xhc3M9ImVmZmVjdC1wb2ludCIgY3g9IjUzNS44NzYzMDA5MjE3OTYiIGN5PSI0NC43NTE3MDk3ODI5MzIiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iNTcyIiBjeT0iMzAuMDAwMDAwMDAwMDAwMSIgcj0iMy41Ij48L2NpcmNsZT48dGV4dCB4PSIzMDAiIHk9IjI4MCIgY2xhc3M9ImF4aXMtbGFiZWwiIHRleHQtYW5jaG9yPSJtaWRkbGUiPmRpc3RhbmNlX2ttPC90ZXh0PjxsaW5lIGNsYXNzPSJzdXBwb3J0LWJhciIgeDE9IjcwIiB4Mj0iNzAiIHkxPSIzMTAiIHkyPSIyOTAiPjwvbGluZT48bGluZSBjbGFzcz0ic3VwcG9ydC1iYXIiIHgxPSI5OC4wNjMwMzg5NTMzMTU1IiB4Mj0iOTguMDYzMDM4OTUzMzE1NSIgeTE9IjMxMCIgeTI9IjI5MCI+PC9saW5lPjxsaW5lIGNsYXNzPSJzdXBwb3J0LWJhciIgeDE9IjEzMy4xNDE4Mzc2NDQ5NiIgeDI9IjEzMy4xNDE4Mzc2NDQ5NiIgeTE9IjMxMCIgeTI9IjI5NCI+PC9saW5lPjxsaW5lIGNsYXNzPSJzdXBwb3J0LWJhciIgeDE9IjE1Mi40NzI0OTQ3OTYzMTMiIHgyPSIxNTIuNDcyNDk0Nzk2MzEzIiB5MT0iMzEwIiB5Mj0iMjkwIj48L2xpbmU+PGxpbmUgY2xhc3M9InN1cHBvcnQtYmFyIiB4MT0iMTc4LjY2OTY0MDIwMjIiIHgyPSIxNzguNjY5NjQwMjAyMiIgeTE9IjMxMCIgeTI9IjI5NCI+PC9saW5lPjxsaW5lIGNsYXNzPSJzdXBwb3J0LWJhciIgeDE9IjI0NC43MjIyNzE3ODExNDgiIHgyPSIyNDQuNzIyMjcxNzgxMTQ4IiB5MT0iMzEwIiB5Mj0iMjkwIj48L2xpbmU+PGxpbmUgY2xhc3M9InN1cHBvcnQtYmFyIiB4MT0iMjY5LjIwMjc5NTEyMzQwMiIgeDI9IjI2OS4yMDI3OTUxMjM0MDIiIHkxPSIzMTAiIHkyPSIyOTQiPjwvbGluZT48bGluZSBjbGFzcz0ic3VwcG9ydC1iYXIiIHgxPSIzNDIuNDk1MDkzNjY2MzY5IiB4Mj0iMzQyLjQ5NTA5MzY2NjM2OSIgeTE9IjMxMCIgeTI9IjI5MCI+PC9saW5lPjxsaW5lIGNsYXNzPSJzdXBwb3J0LWJhciIgeDE9IjM1OC45ODk1OTI2MjU2MzIiIHgyPSIzNTguOTg5NTkyNjI1NjMyIiB5MT0iMzEwIiB5Mj0iMjk0Ij48L2xpbmU+PGxpbmUgY2xhc3M9InN1cHBvcnQtYmFyIiB4MT0iMzk4Ljk5NDM1MDI4MjQ4NiIgeDI9IjM5OC45OTQzNTAyODI0ODYiIHkxPSIzMTAiIHkyPSIyOTAiPjwvbGluZT48bGluZSBjbGFzcz0ic3VwcG9ydC1iYXIiIHgxPSI0MjkuODkzNTQ3NDI3ODkyIiB4Mj0iNDI5Ljg5MzU0NzQyNzg5MiIgeTE9IjMxMCIgeTI9IjI5NCI+PC9saW5lPjxsaW5lIGNsYXNzPSJzdXBwb3J0LWJhciIgeDE9IjQ1Mi4zNTg5MDU3Mzg5MjQiIHgyPSI0NTIuMzU4OTA1NzM4OTI0IiB5MT0iMzEwIiB5Mj0iMjkwIj48L2xpbmU+PGxpbmUgY2xhc3M9InN1cHBvcnQtYmFyIiB4MT0iNDU4LjE4MDQ5MzYwNjg5OSIgeDI9IjQ1OC4xODA0OTM2MDY4OTkiIHkxPSIzMTAiIHkyPSIyOTQiPjwvbGluZT48bGluZSBjbGFzcz0ic3VwcG9ydC1iYXIiIHgxPSI0ODkuNDUyODY5NDYxNzkiIHgyPSI0ODkuNDUyODY5NDYxNzkiIHkxPSIzMTAiIHkyPSIyOTAiPjwvbGluZT48bGluZSBjbGFzcz0ic3VwcG9ydC1iYXIiIHgxPSI1MDcuNzM4NjI2MjI2NTgzIiB4Mj0iNTA3LjczODYyNjIyNjU4MyIgeTE9IjMxMCIgeTI9IjI5NCI+PC9saW5lPjxsaW5lIGNsYXNzPSJzdXBwb3J0LWJhciIgeDE9IjUzNS44NzYzMDA5MjE3OTYiIHgyPSI1MzUuODc2MzAwOTIxNzk2IiB5MT0iMzEwIiB5Mj0iMjkwIj48L2xpbmU+PGxpbmUgY2xhc3M9InN1cHBvcnQtYmFyIiB4MT0iNTcyIiB4Mj0iNTcyIiB5MT0iMzEwIiB5Mj0iMjk0Ij48L2xpbmU+PHRleHQgeD0iNzAiIHk9IjMzOCIgY2xhc3M9InRpY2siPlJlbGF0aXZlIHN1cHBvcnQgMOKAkzE8L3RleHQ+PC9zdmc+)

Support shows relative observed counts (0–1). Descriptive fixed-model
bands propagated from within-bin variation in local prediction
differences under an independent-bin approximation; unavailable if a bin
has fewer than two rows and not model-fitting uncertainty, population
confidence, or causal intervals.

Values, support and descriptive intervals for distance_km

| distance_km | accumulated_effect | std_error | conf_low | conf_high | n   | support |
|-------------|--------------------|-----------|----------|-----------|-----|---------|
| 11.10       | -6.368             | 0.00      | -6.368   | -6.368    | NA  | 1.00    |
| 48.70       | -5.699             | 0.00      | -5.699   | -5.699    | 5   | 1.00    |
| 95.70       | -4.863             | 0.00      | -4.863   | -4.863    | 4   | 0.80    |
| 121.60      | -4.403             | 0.00      | -4.403   | -4.403    | 5   | 1.00    |
| 156.70      | -3.778             | 0.00      | -3.778   | -3.778    | 4   | 0.80    |
| 245.20      | -2.204             | 0.00      | -2.204   | -2.204    | 5   | 1.00    |
| 278.00      | -1.62              | 0.00      | -1.62    | -1.62     | 4   | 0.80    |
| 376.20      | 0.127              | 0.00      | 0.127    | 0.127     | 5   | 1.00    |
| 398.30      | 0.52               | 0.00      | 0.52     | 0.52      | 4   | 0.80    |
| 451.90      | 1.474              | 0.00      | 1.474    | 1.474     | 5   | 1.00    |
| 493.30      | 2.21               | 0.00      | 2.21     | 2.21      | 4   | 0.80    |
| 523.40      | 2.746              | 0.00      | 2.746    | 2.746     | 5   | 1.00    |
| 531.20      | 2.885              | 0.00      | 2.885    | 2.885     | 4   | 0.80    |
| 573.10      | 3.63               | 0.00      | 3.63     | 3.63      | 5   | 1.00    |
| 597.60      | 4.066              | 0.00      | 4.066    | 4.066     | 4   | 0.80    |
| 635.30      | 4.737              | 0.00      | 4.737    | 4.737     | 5   | 1.00    |
| 683.70      | 5.598              | 0.00      | 5.598    | 5.598     | 4   | 0.80    |

ALE values for distance_km ; predicted value ; hours

### planned_route_hours

ALE: centered change in predicted value · hours

Centered fitted effects for predicted value range from -6.36 to 5.511.
Read changes against the input values and support below.

![](data:image/svg+xml;base64,PHN2ZyBjbGFzcz0iZWZmZWN0LXBsb3QiIHN0eWxlPSJtaW4td2lkdGg6NjAwcHgiIHZpZXdib3g9IjAgMCA2MDAgMzUwIiByb2xlPSJpbWciIGFyaWEtbGFiZWw9IkFMRSBmb3IgcGxhbm5lZF9yb3V0ZV9ob3VycyA6IGlucHV0IHZhbHVlcywgZml0dGVkIGVmZmVjdCwgemVybyByZWZlcmVuY2UgZm9yIEFMRSwgYW5kIHJlbGF0aXZlIHN1cHBvcnQuIEZ1bGwgdmFsdWVzIGluIHRoZSBmb2xsb3dpbmcgdGFibGUuIj48dGV4dCB4PSI3MCIgeT0iMTgiIGNsYXNzPSJheGlzLWxhYmVsIj5DZW50ZXJlZCBmaXR0ZWQgZWZmZWN0PC90ZXh0Pjxwb2x5Z29uIGNsYXNzPSJlZmZlY3QtYmFuZCIgcG9pbnRzPSI3MCwyMzUgMTA4LjQ1NjQ3NDI3Mjc0NSwyMTkuMjk1NjYyODk2NTg4IDEzNi4yMDA2MjgyNDA2NjcsMjA3Ljk2NTg3ODkwNTcwNCAxNjYuMjQzODg5NDc0ODcyLDE5NS42OTcyMTY0NDk1MDUgMTg2LjE4MjY2MzM2NjU0MiwxODcuNTU0ODg4NDY1ODU0IDIzOC4wNjkxNjc4NjE5MjQsMTY2LjM2NjE3NjQ3MDcyOCAyNzUuMzA3NjIwNjQ2MSwxNTEuMTU5MjM4NTgwNzc2IDM0NS4yNDk2ODM5NzgyMzIsMTIyLjU5NzI0MDYwNjQ5OSAzNzEuNjUwOTM1MDgwMDA4LDExMS44MTU4NTMyMDQzNzkgMzk5LjIyNTg3Nzc4ODUwOCwxMDAuNTU1MTY5NDI4OTk2IDQyNC40NTY0MDA0MzU1ODksOTAuMjUxODY4MzQ4MDE2NiA0NDguOTE1MTAyNDU0Nzg0LDgwLjI2Mzc1Mjk4MTYxMjIgNDU3LjE5NjQxMDYxMzE2OSw3Ni44ODE5NDM4NzMxMDg0IDQ4NC4wOTczMTM0MTgwMTEsNjUuODk2NTE1NDM2ODY4IDUwNC40NDI2OTY2MTk1MjIsNTcuNTg4MTQxODE4NzIxMiA1MzUuMTY3OTg1MTQwNDQ5LDQ1LjA0MDk2MjI0MzQ0MjIgNTcyLDMwLjAwMDAwMDAwMDAwMDEgNTcyLDMwIDUzNS4xNjc5ODUxNDA0NDksNDUuMDQwOTYyMjQzNDQyIDUwNC40NDI2OTY2MTk1MjIsNTcuNTg4MTQxODE4NzIxMSA0ODQuMDk3MzEzNDE4MDExLDY1Ljg5NjUxNTQzNjg2NzkgNDU3LjE5NjQxMDYxMzE2OSw3Ni44ODE5NDM4NzMxMDgzIDQ0OC45MTUxMDI0NTQ3ODQsODAuMjYzNzUyOTgxNjEyMSA0MjQuNDU2NDAwNDM1NTg5LDkwLjI1MTg2ODM0ODAxNjYgMzk5LjIyNTg3Nzc4ODUwOCwxMDAuNTU1MTY5NDI4OTk2IDM3MS42NTA5MzUwODAwMDgsMTExLjgxNTg1MzIwNDM3OSAzNDUuMjQ5NjgzOTc4MjMyLDEyMi41OTcyNDA2MDY0OTkgMjc1LjMwNzYyMDY0NjEsMTUxLjE1OTIzODU4MDc3NiAyMzguMDY5MTY3ODYxOTI0LDE2Ni4zNjYxNzY0NzA3MjggMTg2LjE4MjY2MzM2NjU0MiwxODcuNTU0ODg4NDY1ODU0IDE2Ni4yNDM4ODk0NzQ4NzIsMTk1LjY5NzIxNjQ0OTUwNSAxMzYuMjAwNjI4MjQwNjY3LDIwNy45NjU4Nzg5MDU3MDQgMTA4LjQ1NjQ3NDI3Mjc0NSwyMTkuMjk1NjYyODk2NTg4IDcwLDIzNSI+PC9wb2x5Z29uPjx0ZXh0IHg9IjE2NC41NDI1ODIyNzYyNTYiIHk9IjI1NyIgY2xhc3M9InRpY2siIHRleHQtYW5jaG9yPSJtaWRkbGUiPjUuMDA8L3RleHQ+PHRleHQgeD0iMzA0LjkzMjc4MDk5NTczMSIgeT0iMjU3IiBjbGFzcz0idGljayIgdGV4dC1hbmNob3I9Im1pZGRsZSI+MTAuMDA8L3RleHQ+PHRleHQgeD0iNDQ1LjMyMjk3OTcxNTIwNiIgeT0iMjU3IiBjbGFzcz0idGljayIgdGV4dC1hbmNob3I9Im1pZGRsZSI+MTUuMDA8L3RleHQ+PGxpbmUgY2xhc3M9ImdyaWQtbGluZSIgeDE9IjcwIiB4Mj0iNTcyIiB5MT0iMjExLjUxMzEwMTkyMjI0MyIgeTI9IjIxMS41MTMxMDE5MjIyNDMiPjwvbGluZT48dGV4dCB4PSI2MCIgeT0iMjE2LjUxMzEwMTkyMjI0MyIgdGV4dC1hbmNob3I9ImVuZCIgY2xhc3M9InRpY2siPi01LjAwPC90ZXh0PjxsaW5lIGNsYXNzPSJncmlkLWxpbmUiIHgxPSI3MCIgeDI9IjU3MiIgeTE9IjEyNS4xNjY5NjkzMjExOTEiIHkyPSIxMjUuMTY2OTY5MzIxMTkxIj48L2xpbmU+PHRleHQgeD0iNjAiIHk9IjEzMC4xNjY5NjkzMjExOTEiIHRleHQtYW5jaG9yPSJlbmQiIGNsYXNzPSJ0aWNrIj4wLjAwPC90ZXh0PjxsaW5lIGNsYXNzPSJncmlkLWxpbmUiIHgxPSI3MCIgeDI9IjU3MiIgeTE9IjM4LjgyMDgzNjcyMDEzOTQiIHkyPSIzOC44MjA4MzY3MjAxMzk0Ij48L2xpbmU+PHRleHQgeD0iNjAiIHk9IjQzLjgyMDgzNjcyMDEzOTQiIHRleHQtYW5jaG9yPSJlbmQiIGNsYXNzPSJ0aWNrIj41LjAwPC90ZXh0PjxsaW5lIGNsYXNzPSJ6ZXJvLWxpbmUiIHgxPSI3MCIgeDI9IjU3MiIgeTE9IjEyNS4xNjY5NjkzMjExOTEiIHkyPSIxMjUuMTY2OTY5MzIxMTkxIj48L2xpbmU+PHRleHQgeD0iNTcyIiB5PSIxMTkuMTY2OTY5MzIxMTkxIiBjbGFzcz0idGljayIgdGV4dC1hbmNob3I9ImVuZCI+emVybzwvdGV4dD48cG9seWxpbmUgY2xhc3M9ImVmZmVjdC1saW5lIiBwb2ludHM9IjcwLDIzNSAxMDguNDU2NDc0MjcyNzQ1LDIxOS4yOTU2NjI4OTY1ODggMTM2LjIwMDYyODI0MDY2NywyMDcuOTY1ODc4OTA1NzA0IDE2Ni4yNDM4ODk0NzQ4NzIsMTk1LjY5NzIxNjQ0OTUwNSAxODYuMTgyNjYzMzY2NTQyLDE4Ny41NTQ4ODg0NjU4NTQgMjM4LjA2OTE2Nzg2MTkyNCwxNjYuMzY2MTc2NDcwNzI4IDI3NS4zMDc2MjA2NDYxLDE1MS4xNTkyMzg1ODA3NzYgMzQ1LjI0OTY4Mzk3ODIzMiwxMjIuNTk3MjQwNjA2NDk5IDM3MS42NTA5MzUwODAwMDgsMTExLjgxNTg1MzIwNDM3OSAzOTkuMjI1ODc3Nzg4NTA4LDEwMC41NTUxNjk0Mjg5OTYgNDI0LjQ1NjQwMDQzNTU4OSw5MC4yNTE4NjgzNDgwMTY2IDQ0OC45MTUxMDI0NTQ3ODQsODAuMjYzNzUyOTgxNjEyMiA0NTcuMTk2NDEwNjEzMTY5LDc2Ljg4MTk0Mzg3MzEwODQgNDg0LjA5NzMxMzQxODAxMSw2NS44OTY1MTU0MzY4NjggNTA0LjQ0MjY5NjYxOTUyMiw1Ny41ODgxNDE4MTg3MjEyIDUzNS4xNjc5ODUxNDA0NDksNDUuMDQwOTYyMjQzNDQyMSA1NzIsMzAuMDAwMDAwMDAwMDAwMSI+PC9wb2x5bGluZT48Y2lyY2xlIGNsYXNzPSJlZmZlY3QtcG9pbnQiIGN4PSI3MCIgY3k9IjIzNSIgcj0iMy41Ij48L2NpcmNsZT48Y2lyY2xlIGNsYXNzPSJlZmZlY3QtcG9pbnQiIGN4PSIxMDguNDU2NDc0MjcyNzQ1IiBjeT0iMjE5LjI5NTY2Mjg5NjU4OCIgcj0iMy41Ij48L2NpcmNsZT48Y2lyY2xlIGNsYXNzPSJlZmZlY3QtcG9pbnQiIGN4PSIxMzYuMjAwNjI4MjQwNjY3IiBjeT0iMjA3Ljk2NTg3ODkwNTcwNCIgcj0iMy41Ij48L2NpcmNsZT48Y2lyY2xlIGNsYXNzPSJlZmZlY3QtcG9pbnQiIGN4PSIxNjYuMjQzODg5NDc0ODcyIiBjeT0iMTk1LjY5NzIxNjQ0OTUwNSIgcj0iMy41Ij48L2NpcmNsZT48Y2lyY2xlIGNsYXNzPSJlZmZlY3QtcG9pbnQiIGN4PSIxODYuMTgyNjYzMzY2NTQyIiBjeT0iMTg3LjU1NDg4ODQ2NTg1NCIgcj0iMy41Ij48L2NpcmNsZT48Y2lyY2xlIGNsYXNzPSJlZmZlY3QtcG9pbnQiIGN4PSIyMzguMDY5MTY3ODYxOTI0IiBjeT0iMTY2LjM2NjE3NjQ3MDcyOCIgcj0iMy41Ij48L2NpcmNsZT48Y2lyY2xlIGNsYXNzPSJlZmZlY3QtcG9pbnQiIGN4PSIyNzUuMzA3NjIwNjQ2MSIgY3k9IjE1MS4xNTkyMzg1ODA3NzYiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iMzQ1LjI0OTY4Mzk3ODIzMiIgY3k9IjEyMi41OTcyNDA2MDY0OTkiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iMzcxLjY1MDkzNTA4MDAwOCIgY3k9IjExMS44MTU4NTMyMDQzNzkiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iMzk5LjIyNTg3Nzc4ODUwOCIgY3k9IjEwMC41NTUxNjk0Mjg5OTYiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iNDI0LjQ1NjQwMDQzNTU4OSIgY3k9IjkwLjI1MTg2ODM0ODAxNjYiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iNDQ4LjkxNTEwMjQ1NDc4NCIgY3k9IjgwLjI2Mzc1Mjk4MTYxMjIiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iNDU3LjE5NjQxMDYxMzE2OSIgY3k9Ijc2Ljg4MTk0Mzg3MzEwODQiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iNDg0LjA5NzMxMzQxODAxMSIgY3k9IjY1Ljg5NjUxNTQzNjg2OCIgcj0iMy41Ij48L2NpcmNsZT48Y2lyY2xlIGNsYXNzPSJlZmZlY3QtcG9pbnQiIGN4PSI1MDQuNDQyNjk2NjE5NTIyIiBjeT0iNTcuNTg4MTQxODE4NzIxMiIgcj0iMy41Ij48L2NpcmNsZT48Y2lyY2xlIGNsYXNzPSJlZmZlY3QtcG9pbnQiIGN4PSI1MzUuMTY3OTg1MTQwNDQ5IiBjeT0iNDUuMDQwOTYyMjQzNDQyMSIgcj0iMy41Ij48L2NpcmNsZT48Y2lyY2xlIGNsYXNzPSJlZmZlY3QtcG9pbnQiIGN4PSI1NzIiIGN5PSIzMC4wMDAwMDAwMDAwMDAxIiByPSIzLjUiPjwvY2lyY2xlPjx0ZXh0IHg9IjMwMCIgeT0iMjgwIiBjbGFzcz0iYXhpcy1sYWJlbCIgdGV4dC1hbmNob3I9Im1pZGRsZSI+cGxhbm5lZF9yb3V0ZV9ob3VyczwvdGV4dD48bGluZSBjbGFzcz0ic3VwcG9ydC1iYXIiIHgxPSI3MCIgeDI9IjcwIiB5MT0iMzEwIiB5Mj0iMjkwIj48L2xpbmU+PGxpbmUgY2xhc3M9InN1cHBvcnQtYmFyIiB4MT0iMTA4LjQ1NjQ3NDI3Mjc0NSIgeDI9IjEwOC40NTY0NzQyNzI3NDUiIHkxPSIzMTAiIHkyPSIyOTAiPjwvbGluZT48bGluZSBjbGFzcz0ic3VwcG9ydC1iYXIiIHgxPSIxMzYuMjAwNjI4MjQwNjY3IiB4Mj0iMTM2LjIwMDYyODI0MDY2NyIgeTE9IjMxMCIgeTI9IjI5NCI+PC9saW5lPjxsaW5lIGNsYXNzPSJzdXBwb3J0LWJhciIgeDE9IjE2Ni4yNDM4ODk0NzQ4NzIiIHgyPSIxNjYuMjQzODg5NDc0ODcyIiB5MT0iMzEwIiB5Mj0iMjkwIj48L2xpbmU+PGxpbmUgY2xhc3M9InN1cHBvcnQtYmFyIiB4MT0iMTg2LjE4MjY2MzM2NjU0MiIgeDI9IjE4Ni4xODI2NjMzNjY1NDIiIHkxPSIzMTAiIHkyPSIyOTQiPjwvbGluZT48bGluZSBjbGFzcz0ic3VwcG9ydC1iYXIiIHgxPSIyMzguMDY5MTY3ODYxOTI0IiB4Mj0iMjM4LjA2OTE2Nzg2MTkyNCIgeTE9IjMxMCIgeTI9IjI5MCI+PC9saW5lPjxsaW5lIGNsYXNzPSJzdXBwb3J0LWJhciIgeDE9IjI3NS4zMDc2MjA2NDYxIiB4Mj0iMjc1LjMwNzYyMDY0NjEiIHkxPSIzMTAiIHkyPSIyOTQiPjwvbGluZT48bGluZSBjbGFzcz0ic3VwcG9ydC1iYXIiIHgxPSIzNDUuMjQ5NjgzOTc4MjMyIiB4Mj0iMzQ1LjI0OTY4Mzk3ODIzMiIgeTE9IjMxMCIgeTI9IjI5MCI+PC9saW5lPjxsaW5lIGNsYXNzPSJzdXBwb3J0LWJhciIgeDE9IjM3MS42NTA5MzUwODAwMDgiIHgyPSIzNzEuNjUwOTM1MDgwMDA4IiB5MT0iMzEwIiB5Mj0iMjk0Ij48L2xpbmU+PGxpbmUgY2xhc3M9InN1cHBvcnQtYmFyIiB4MT0iMzk5LjIyNTg3Nzc4ODUwOCIgeDI9IjM5OS4yMjU4Nzc3ODg1MDgiIHkxPSIzMTAiIHkyPSIyOTAiPjwvbGluZT48bGluZSBjbGFzcz0ic3VwcG9ydC1iYXIiIHgxPSI0MjQuNDU2NDAwNDM1NTg5IiB4Mj0iNDI0LjQ1NjQwMDQzNTU4OSIgeTE9IjMxMCIgeTI9IjI5NCI+PC9saW5lPjxsaW5lIGNsYXNzPSJzdXBwb3J0LWJhciIgeDE9IjQ0OC45MTUxMDI0NTQ3ODQiIHgyPSI0NDguOTE1MTAyNDU0Nzg0IiB5MT0iMzEwIiB5Mj0iMjkwIj48L2xpbmU+PGxpbmUgY2xhc3M9InN1cHBvcnQtYmFyIiB4MT0iNDU3LjE5NjQxMDYxMzE2OSIgeDI9IjQ1Ny4xOTY0MTA2MTMxNjkiIHkxPSIzMTAiIHkyPSIyOTQiPjwvbGluZT48bGluZSBjbGFzcz0ic3VwcG9ydC1iYXIiIHgxPSI0ODQuMDk3MzEzNDE4MDExIiB4Mj0iNDg0LjA5NzMxMzQxODAxMSIgeTE9IjMxMCIgeTI9IjI5MCI+PC9saW5lPjxsaW5lIGNsYXNzPSJzdXBwb3J0LWJhciIgeDE9IjUwNC40NDI2OTY2MTk1MjIiIHgyPSI1MDQuNDQyNjk2NjE5NTIyIiB5MT0iMzEwIiB5Mj0iMjk0Ij48L2xpbmU+PGxpbmUgY2xhc3M9InN1cHBvcnQtYmFyIiB4MT0iNTM1LjE2Nzk4NTE0MDQ0OSIgeDI9IjUzNS4xNjc5ODUxNDA0NDkiIHkxPSIzMTAiIHkyPSIyOTAiPjwvbGluZT48bGluZSBjbGFzcz0ic3VwcG9ydC1iYXIiIHgxPSI1NzIiIHgyPSI1NzIiIHkxPSIzMTAiIHkyPSIyOTQiPjwvbGluZT48dGV4dCB4PSI3MCIgeT0iMzM4IiBjbGFzcz0idGljayI+UmVsYXRpdmUgc3VwcG9ydCAw4oCTMTwvdGV4dD48L3N2Zz4=)

Support shows relative observed counts (0–1). Descriptive fixed-model
bands propagated from within-bin variation in local prediction
differences under an independent-bin approximation; unavailable if a bin
has fewer than two rows and not model-fitting uncertainty, population
confidence, or causal intervals.

Values, support and descriptive intervals for planned_route_hours

| planned_route_hours | accumulated_effect | std_error | conf_low | conf_high | n | support |
|----|----|----|----|----|----|----|
| 1.633 | -6.36 | 0.00 | -6.36 | -6.36 | NA | 1.00 |
| 3.002 | -5.451 | 0.00 | -5.451 | -5.451 | 5 | 1.00 |
| 3.991 | -4.795 | 0.00 | -4.795 | -4.795 | 4 | 0.80 |
| 5.061 | -4.084 | 0.00 | -4.084 | -4.084 | 5 | 1.00 |
| 5.771 | -3.613 | 0.00 | -3.613 | -3.613 | 4 | 0.80 |
| 7.619 | -2.386 | 0.00 | -2.386 | -2.386 | 5 | 1.00 |
| 8.945 | -1.505 | 0.00 | -1.505 | -1.505 | 4 | 0.80 |
| 11.436 | 0.149 | 0.00 | 0.149 | 0.149 | 5 | 1.00 |
| 12.376 | 0.773 | 0.00 | 0.773 | 0.773 | 4 | 0.80 |
| 13.358 | 1.425 | 0.00 | 1.425 | 1.425 | 5 | 1.00 |
| 14.257 | 2.022 | 0.00 | 2.022 | 2.022 | 4 | 0.80 |
| 15.128 | 2.60 | 0.00 | 2.60 | 2.60 | 5 | 1.00 |
| 15.423 | 2.796 | 0.00 | 2.796 | 2.796 | 4 | 0.80 |
| 16.381 | 3.432 | 0.00 | 3.432 | 3.432 | 5 | 1.00 |
| 17.106 | 3.913 | 0.00 | 3.913 | 3.913 | 4 | 0.80 |
| 18.20 | 4.64 | 0.00 | 4.64 | 4.64 | 5 | 1.00 |
| 19.512 | 5.511 | 0.00 | 5.511 | 5.511 | 4 | 0.80 |

ALE values for planned_route_hours ; predicted value ; hours

### service

PDP: average predicted value · hours

Average fitted predictions for predicted value range from 21.902 to
26.902. Read changes against the input values and support below.

![](data:image/svg+xml;base64,PHN2ZyBjbGFzcz0iZWZmZWN0LXBsb3QiIHN0eWxlPSJtaW4td2lkdGg6NjAwcHgiIHZpZXdib3g9IjAgMCA2MDAgMzUwIiByb2xlPSJpbWciIGFyaWEtbGFiZWw9IlBEUCBmb3Igc2VydmljZSA6IGlucHV0IHZhbHVlcywgZml0dGVkIGVmZmVjdCwgemVybyByZWZlcmVuY2UgZm9yIEFMRSwgYW5kIHJlbGF0aXZlIHN1cHBvcnQuIEZ1bGwgdmFsdWVzIGluIHRoZSBmb2xsb3dpbmcgdGFibGUuIj48dGV4dCB4PSI3MCIgeT0iMTgiIGNsYXNzPSJheGlzLWxhYmVsIj5BdmVyYWdlIHByZWRpY3Rpb248L3RleHQ+PHRleHQgeD0iMTk1LjUiIHk9IjI1NyIgY2xhc3M9InRpY2siIHRleHQtYW5jaG9yPSJtaWRkbGUiPmVjb25vbXk8L3RleHQ+PHRleHQgeD0iNDQ2LjUiIHk9IjI1NyIgY2xhc3M9InRpY2siIHRleHQtYW5jaG9yPSJtaWRkbGUiPnByaW9yaXR5PC90ZXh0PjxsaW5lIGNsYXNzPSJncmlkLWxpbmUiIHgxPSI3MCIgeDI9IjU3MiIgeTE9IjE5Mi4wMTI4NTY3MzY2MiIgeTI9IjE5Mi4wMTI4NTY3MzY2MiI+PC9saW5lPjx0ZXh0IHg9IjYwIiB5PSIxOTcuMDEyODU2NzM2NjIiIHRleHQtYW5jaG9yPSJlbmQiIGNsYXNzPSJ0aWNrIj4yMi4wMDwvdGV4dD48bGluZSBjbGFzcz0iZ3JpZC1saW5lIiB4MT0iNzAiIHgyPSI1NzIiIHkxPSIxNDIuNDU4MjI2NjkwNzQiIHkyPSIxNDIuNDU4MjI2NjkwNzQiPjwvbGluZT48dGV4dCB4PSI2MCIgeT0iMTQ3LjQ1ODIyNjY5MDc0IiB0ZXh0LWFuY2hvcj0iZW5kIiBjbGFzcz0idGljayI+MjQuMDA8L3RleHQ+PGxpbmUgY2xhc3M9ImdyaWQtbGluZSIgeDE9IjcwIiB4Mj0iNTcyIiB5MT0iOTIuOTAzNTk2NjQ0ODU5IiB5Mj0iOTIuOTAzNTk2NjQ0ODU5Ij48L2xpbmU+PHRleHQgeD0iNjAiIHk9Ijk3LjkwMzU5NjY0NDg1OSIgdGV4dC1hbmNob3I9ImVuZCIgY2xhc3M9InRpY2siPjI2LjAwPC90ZXh0PjxsaW5lIGNsYXNzPSJncmlkLWxpbmUiIHgxPSI3MCIgeDI9IjU3MiIgeTE9IjQzLjM0ODk2NjU5ODk3ODMiIHkyPSI0My4zNDg5NjY1OTg5NzgzIj48L2xpbmU+PHRleHQgeD0iNjAiIHk9IjQ4LjM0ODk2NjU5ODk3ODMiIHRleHQtYW5jaG9yPSJlbmQiIGNsYXNzPSJ0aWNrIj4yOC4wMDwvdGV4dD48Y2lyY2xlIGNsYXNzPSJlZmZlY3QtcG9pbnQiIGN4PSIxOTUuNSIgY3k9IjcwLjU0OTEzNjM5OTM5OTIiIHI9IjMuNSI+PC9jaXJjbGU+PGNpcmNsZSBjbGFzcz0iZWZmZWN0LXBvaW50IiBjeD0iNDQ2LjUiIGN5PSIxOTQuNDUwODYzNjAwNjAxIiByPSIzLjUiPjwvY2lyY2xlPjx0ZXh0IHg9IjMwMCIgeT0iMjgwIiBjbGFzcz0iYXhpcy1sYWJlbCIgdGV4dC1hbmNob3I9Im1pZGRsZSI+c2VydmljZTwvdGV4dD48bGluZSBjbGFzcz0ic3VwcG9ydC1iYXIiIHgxPSIxOTUuNSIgeDI9IjE5NS41IiB5MT0iMzEwIiB5Mj0iMjkwIj48L2xpbmU+PGxpbmUgY2xhc3M9InN1cHBvcnQtYmFyIiB4MT0iNDQ2LjUiIHgyPSI0NDYuNSIgeTE9IjMxMCIgeTI9IjI5Mi4xMDUyNjMxNTc4OTUiPjwvbGluZT48dGV4dCB4PSI3MCIgeT0iMzM4IiBjbGFzcz0idGljayI+UmVsYXRpdmUgc3VwcG9ydCAw4oCTMTwvdGV4dD48L3N2Zz4=)

Support shows relative observed counts (0–1). Descriptive fixed-model
bands from across-row prediction variation at each grid value; not
model-fitting uncertainty, population confidence, or causal intervals.

Values, support and descriptive intervals for service

| service  | partial_dependence | std_error | conf_low | conf_high | support |
|----------|--------------------|-----------|----------|-----------|---------|
| economy  | 26.902             | 0.835     | 25.266   | 28.539    | 1.00    |
| priority | 21.902             | 0.835     | 20.265   | 23.538    | 0.895   |

PDP values for service ; predicted value ; hours

All model-feature evidence and shuffle intervals

### main_model

| Feature | Loss change | MC interval | Shuffle result | Interpretation |
|----|----|----|----|----|
| distance_km | 2.3029 | \[2.185, 2.4209\] | positive loss change | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |
| planned_route_hours | 2.1173 | \[1.982, 2.2526\] | positive loss change | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |
| service | 1.9869 | \[1.9072, 2.0666\] | positive loss change | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |
| dispatch_backlog | 0.4064 | \[0.3483, 0.4645\] | positive loss change | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |
| parcel_kg | 0.174 | \[0.1421, 0.2059\] | positive loss change | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |

Shuffle evidence for main_model

### simple_baseline

| Feature | Loss change | MC interval | Shuffle result | Interpretation |
|----|----|----|----|----|
| distance_km | 0.00 | \[0.00, 0.00\] | no observed change | No loss change in these shuffles; this is not proof of no population importance. |
| planned_route_hours | 0.00 | \[0.00, 0.00\] | no observed change | No loss change in these shuffles; this is not proof of no population importance. |
| service | 0.00 | \[0.00, 0.00\] | no observed change | No loss change in these shuffles; this is not proof of no population importance. |
| dispatch_backlog | 0.00 | \[0.00, 0.00\] | no observed change | No loss change in these shuffles; this is not proof of no population importance. |
| parcel_kg | 0.00 | \[0.00, 0.00\] | no observed change | No loss change in these shuffles; this is not proof of no population importance. |

Shuffle evidence for simple_baseline

### small_tree

| Feature | Loss change | MC interval | Shuffle result | Interpretation |
|----|----|----|----|----|
| distance_km | 4.2993 | \[4.08, 4.5187\] | positive loss change | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |
| planned_route_hours | 0.9635 | \[0.8636, 1.0633\] | positive loss change | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |
| service | 0.00 | \[0.00, 0.00\] | no observed change | No loss change in these shuffles; this is not proof of no population importance. |
| dispatch_backlog | 0.00 | \[0.00, 0.00\] | no observed change | No loss change in these shuffles; this is not proof of no population importance. |
| parcel_kg | 0.00 | \[0.00, 0.00\] | no observed change | No loss change in these shuffles; this is not proof of no population importance. |

Shuffle evidence for small_tree

### flexible_tree

| Feature | Loss change | MC interval | Shuffle result | Interpretation |
|----|----|----|----|----|
| distance_km | 4.2698 | \[4.0977, 4.4419\] | positive loss change | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |
| service | 1.3185 | \[1.2247, 1.4124\] | positive loss change | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |
| planned_route_hours | 0.8007 | \[0.6746, 0.9267\] | positive loss change | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |
| dispatch_backlog | 0.207 | \[0.1647, 0.2493\] | positive loss change | Shuffling increased loss; the fixed-sample Monte Carlo interval excludes zero. |
| parcel_kg | -0.0871 | \[-0.1339, -0.0404\] | negative loss change | Shuffling reduced loss; the fixed-sample Monte Carlo interval excludes zero. |

Shuffle evidence for flexible_tree

Sensitivity to model choice

## Candidate scores and prediction differences

Pre-specified model; candidate evaluation ranks did not select it.
Candidate scores use the same evaluation rows.

| Model                   | Role      | RMSE  |
|-------------------------|-----------|-------|
| linear regression       | primary   | 2.997 |
| flexible decision tree  | candidate | 4.135 |
| small decision tree     | candidate | 4.797 |
| intercept-only baseline | baseline  | 7.565 |

Scores on common evaluation rows

### Where did supplied model choices disagree?

The same evaluation rows were scored by every supplied non-baseline
candidate. A large gap means the answer depends on model specification,
even when the data row is unchanged.

Compared candidates

**3**Simple baseline excluded

Median prediction range

**3.8996**In outcome units

90th-percentile range

**6.0059**Nine in ten rows were below this

Largest prediction range

**8.0725**Most specification-sensitive row

| Row | Observed | Lowest prediction | Highest prediction | Range  |
|-----|----------|-------------------|--------------------|--------|
| 17  | 13.7486  | 13.7468           | 21.8193            | 8.0725 |
| 29  | 15.3643  | 18.5115           | 26.0662            | 7.5546 |
| 13  | 37.6837  | 32.2623           | 39.5375            | 7.2752 |
| 72  | 33.7778  | 26.0662           | 32.9531            | 6.8869 |
| 21  | 27.4958  | 26.0662           | 32.075             | 6.0089 |

Values: Row, Observed, Lowest prediction

**Disagreement is a review signal, not an error bar.** The compared
candidates can have very different evaluation performance; read this
beside the score table above. It does not identify the correct
prediction or provide uncertainty coverage.

Resource measurements and Pareto comparison

Approximate R object size is an operational measurement; it does not
measure how complex a learned relationship is. Use this comparison only
when the displayed measurement matters to your application. Lower
resource use is to the left; better prediction is higher.

![](data:image/svg+xml;base64,PHN2ZyBjbGFzcz0idHJhZGVvZmYtcGxvdCIgdmlld2JveD0iMCAwIDY4MCAzNDAiIHJvbGU9ImltZyIgYXJpYS1sYWJlbD0iQ2FuZGlkYXRlIFJNU0UgdmVyc3VzIGFwcHJveGltYXRlIG1vZGVsLW9iamVjdCBzaXplIChLQikgLiBOdW1iZXJlZCBwb2ludHMgbWF0Y2ggdGhlIGtleSBiZWxvdy4iPjx0ZXh0IHg9IjEzMi44MjUwNjAyNDA5NjQiIHk9IjI5MyIgY2xhc3M9InRpY2siIHRleHQtYW5jaG9yPSJtaWRkbGUiPjcwLjAwPC90ZXh0Pjx0ZXh0IHg9IjI1MS42NzQ4NTk0Mzc3NTEiIHk9IjI5MyIgY2xhc3M9InRpY2siIHRleHQtYW5jaG9yPSJtaWRkbGUiPjgwLjAwPC90ZXh0Pjx0ZXh0IHg9IjM3MC41MjQ2NTg2MzQ1MzgiIHk9IjI5MyIgY2xhc3M9InRpY2siIHRleHQtYW5jaG9yPSJtaWRkbGUiPjkwLjAwPC90ZXh0Pjx0ZXh0IHg9IjQ4OS4zNzQ0NTc4MzEzMjUiIHk9IjI5MyIgY2xhc3M9InRpY2siIHRleHQtYW5jaG9yPSJtaWRkbGUiPjEwMC4wMDwvdGV4dD48dGV4dCB4PSI2MDguMjI0MjU3MDI4MTEyIiB5PSIyOTMiIGNsYXNzPSJ0aWNrIiB0ZXh0LWFuY2hvcj0ibWlkZGxlIj4xMTAuMDA8L3RleHQ+PGxpbmUgeDE9Ijc2IiB4Mj0iNjU0IiB5MT0iMzIuMTY2MTQ5ODA1MTgxOSIgeTI9IjMyLjE2NjE0OTgwNTE4MTkiIGNsYXNzPSJncmlkLWxpbmUiPjwvbGluZT48dGV4dCB4PSI2NyIgeT0iMzcuMTY2MTQ5ODA1MTgxOSIgY2xhc3M9InRpY2siIHRleHQtYW5jaG9yPSJlbmQiPjMuMDA8L3RleHQ+PGxpbmUgeDE9Ijc2IiB4Mj0iNjU0IiB5MT0iODQuMjcxMTQyMDA4MDY4NiIgeTI9Ijg0LjI3MTE0MjAwODA2ODYiIGNsYXNzPSJncmlkLWxpbmUiPjwvbGluZT48dGV4dCB4PSI2NyIgeT0iODkuMjcxMTQyMDA4MDY4NiIgY2xhc3M9InRpY2siIHRleHQtYW5jaG9yPSJlbmQiPjQuMDA8L3RleHQ+PGxpbmUgeDE9Ijc2IiB4Mj0iNjU0IiB5MT0iMTM2LjM3NjEzNDIxMDk1NSIgeTI9IjEzNi4zNzYxMzQyMTA5NTUiIGNsYXNzPSJncmlkLWxpbmUiPjwvbGluZT48dGV4dCB4PSI2NyIgeT0iMTQxLjM3NjEzNDIxMDk1NSIgY2xhc3M9InRpY2siIHRleHQtYW5jaG9yPSJlbmQiPjUuMDA8L3RleHQ+PGxpbmUgeDE9Ijc2IiB4Mj0iNjU0IiB5MT0iMTg4LjQ4MTEyNjQxMzg0MiIgeTI9IjE4OC40ODExMjY0MTM4NDIiIGNsYXNzPSJncmlkLWxpbmUiPjwvbGluZT48dGV4dCB4PSI2NyIgeT0iMTkzLjQ4MTEyNjQxMzg0MiIgY2xhc3M9InRpY2siIHRleHQtYW5jaG9yPSJlbmQiPjYuMDA8L3RleHQ+PGxpbmUgeDE9Ijc2IiB4Mj0iNjU0IiB5MT0iMjQwLjU4NjExODYxNjcyOSIgeTI9IjI0MC41ODYxMTg2MTY3MjkiIGNsYXNzPSJncmlkLWxpbmUiPjwvbGluZT48dGV4dCB4PSI2NyIgeT0iMjQ1LjU4NjExODYxNjcyOSIgY2xhc3M9InRpY2siIHRleHQtYW5jaG9yPSJlbmQiPjcuMDA8L3RleHQ+PGxpbmUgeDE9Ijc2IiB4Mj0iNjU0IiB5MT0iMjcwIiB5Mj0iMjcwIiBjbGFzcz0iYXhpcyI+PC9saW5lPjx0ZXh0IHg9Ijc2IiB5PSIxOCIgY2xhc3M9ImF4aXMtbGFiZWwiPlJNU0UgKGxvd2VyIGlzIGJldHRlcik8L3RleHQ+PHRleHQgeD0iMzQwIiB5PSIzMjUiIHRleHQtYW5jaG9yPSJtaWRkbGUiIGNsYXNzPSJheGlzLWxhYmVsIj5hcHByb3hpbWF0ZSBtb2RlbC1vYmplY3Qgc2l6ZSAoS0IpIChsb3dlciBpcyBsZWZ0KTwvdGV4dD48cG9seWxpbmUgY2xhc3M9InBhcmV0by1saW5lIiBwb2ludHM9Ijc2LDEyNS43NzM4OTg2NjQzMDkgMjI0LjkzMzY1NDYxODQ3NCw5MS4yODE1MzEwMjM3MDkgNjU0LDMyIj48L3BvbHlsaW5lPjxjaXJjbGUgY3g9IjY1NCIgY3k9IjMyIiByPSI3IiBjbGFzcz0idHJhZGVvZmYtcG9pbnQgdHJhZGVvZmYtcHJpbWFyeSB0cmFkZW9mZi1wYXJldG8iPjwvY2lyY2xlPjx0ZXh0IHg9IjY0MiIgeT0iNTQiIHRleHQtYW5jaG9yPSJlbmQiIGNsYXNzPSJwb2ludC1sYWJlbCI+MTwvdGV4dD48Y2lyY2xlIGN4PSIyMjQuOTMzNjU0NjE4NDc0IiBjeT0iOTEuMjgxNTMxMDIzNzA5IiByPSI3IiBjbGFzcz0idHJhZGVvZmYtcG9pbnQgdHJhZGVvZmYtY2FuZGlkYXRlIHRyYWRlb2ZmLXBhcmV0byI+PC9jaXJjbGU+PHRleHQgeD0iMjM2LjkzMzY1NDYxODQ3NCIgeT0iODEuMjgxNTMxMDIzNzA5IiB0ZXh0LWFuY2hvcj0ic3RhcnQiIGNsYXNzPSJwb2ludC1sYWJlbCI+MjwvdGV4dD48Y2lyY2xlIGN4PSI3NiIgY3k9IjEyNS43NzM4OTg2NjQzMDkiIHI9IjciIGNsYXNzPSJ0cmFkZW9mZi1wb2ludCB0cmFkZW9mZi1jYW5kaWRhdGUgdHJhZGVvZmYtcGFyZXRvIj48L2NpcmNsZT48dGV4dCB4PSI4OCIgeT0iMTE1Ljc3Mzg5ODY2NDMwOSIgdGV4dC1hbmNob3I9InN0YXJ0IiBjbGFzcz0icG9pbnQtbGFiZWwiPjM8L3RleHQ+PGNpcmNsZSBjeD0iMjQ4LjA1MzY1NDYxODQ3NCIgY3k9IjI3MCIgcj0iNyIgY2xhc3M9InRyYWRlb2ZmLXBvaW50IHRyYWRlb2ZmLWJhc2VsaW5lIj48L2NpcmNsZT48dGV4dCB4PSIyNjAuMDUzNjU0NjE4NDc0IiB5PSIyNjAiIHRleHQtYW5jaG9yPSJzdGFydCIgY2xhc3M9InBvaW50LWxhYmVsIj40PC90ZXh0Pjwvc3ZnPg==)

1.  linear regression
2.  flexible decision tree
3.  small decision tree
4.  intercept-only baseline

| Model | Role | RMSE | approximate model-object size (KB) | Pareto-efficient |
|----|----|----|----|----|
| linear regression | primary | 2.997 | 113.852 | yes |
| flexible decision tree | candidate | 4.135 | 77.75 | yes |
| small decision tree | candidate | 4.797 | 65.219 | yes |
| intercept-only baseline | baseline | 7.565 | 79.695 | no |

Resource comparison values

Outlined points are Pareto-efficient among these supplied models on
these two axes. Pareto status compares only the supplied models on the
supplied evaluation data and exact displayed axes. Resource proxies are
not structural complexity; this is not a final model-selection rule.

What each model family can represent

### How are these model families different?

**Prior/model-capacity knowledge:** the nonlinearity and interaction
columns describe each model family. They say what each family can
represent; they do not show that this fitted model actually used those
patterns.

| Model | Family | Backend | Capacity: nonlinearity | Capacity: interactions | Computed RMSE |
|----|----|----|----|----|----|
| linear regression | linear | stats | none unless encoded in features | none unless specified in features | 2.9968 |
| flexible decision tree | tree | rpart | stepwise | automatic along tree paths | 4.1345 |
| small decision tree | tree | rpart | stepwise | automatic along tree paths | 4.7965 |

Values: Model, Family, Backend

**Computed evidence from this analysis:** evaluation performance and
paired prediction disagreement are calculated on common evaluation rows.
Repeated permutation feature importance in the Patterns section is also
computed evidence of model reliance, not a property guaranteed by the
family card.

- linear regression has the best supplied rmse score (2.997).
- flexible decision tree and small decision tree differ most on average
  (3.3 using absolute difference in predicted target units).

## What this analysis does not establish

Evaluation describes these rows under the recorded validation design.
Feature effects describe the fitted model. Changing an input in the real
world need not cause the plotted change. Performance in another
population or future period requires separate evidence, as do fairness,
safety and suitability for deployment.

Reproducibility

## How this result was produced

Keep the fitted result with its data version and analysis code.

    saveRDS(result, "analysis.rds")
    result$evaluation
    result$explanations$audit

Complete run metadata

- Generated: 2026-09-07 11:07:10 UTC
- Package version: 0.4.0
- Engine: base
- Target: delivery_hours
- Task: regression
- Training rows: 288
- Evaluation rows: 72
- Evaluation role: test
- Split method: reproducible random holdout
- Primary model ID: main_model
- Primary model label: linear regression
- Permutation repeats: 20
- Seed: 2026
- Explainer IDs:
  axr-f2f4da582d9808edef404bda985fcea672c641e0edea602b274a09dd6d7a6daa,
  axr-9e5035f40f4310b7a06ef19fbaba28499dbdb41bcc0e5b4186303bdcf50d167f,
  axr-6c638bdd3eef93316b0751fdef456354f38ec4c473aa41ce3e18f5946585f531,
  axr-001d63d475fbc91ae7b948069975f545138d7d761d30a426a69795f4f36de1c3
