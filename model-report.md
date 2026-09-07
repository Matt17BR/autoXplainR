Start here

## The modeling question

This is a **regression** task: predict a numeric value for **outcome**.

The primary model improved RMSE by 66.5% relative to the simple baseline
on the independent test rows.

Primary model

**0.994**RMSE

Simple baseline

**2.9691**Predicts without using input features

Relative improvement

**66.5%**Positive means the primary error was lower

Test rows

**48**Asserted as independent test data

### Important context for these scores

### Only 48 rows were available for test scoring.

**Next step:** Treat the scores as preliminary and validate on more
representative rows.

Independent-test check

## Did the model generalize?

**RMSE:** Typical prediction error, with larger mistakes weighted more
heavily; lower is better.

The table reports every computed test-set metric. Compare models using
the metric definitions, not the rank column alone.

| Rank | Model                   | Role      | RMSE   | MAE    | R-squared |
|------|-------------------------|-----------|--------|--------|-----------|
| 1    | linear regression       | primary   | 0.994  | 0.7766 | 0.8605    |
| 2    | flexible decision tree  | candidate | 1.2197 | 0.9647 | 0.79      |
| 3    | small decision tree     | candidate | 1.4117 | 1.1274 | 0.7187    |
| 4    | intercept-only baseline | baseline  | 2.9691 | 2.4731 | -0.2445   |

Definitions for every metric

- RMSE: Typical prediction error, with larger mistakes weighted more
  heavily; lower is better.
- MAE: Average absolute prediction error in the target's units; lower is
  better.
- R-squared: Share of evaluation-set variation explained relative to
  predicting the evaluation-set mean; higher is better.

### How large were individual errors?

Mean error

**-0.2183**Observed minus predicted; near zero means little average bias

Median absolute error

**0.6203**Half of absolute errors were below this value

90th-percentile error

**1.5355**Nine in ten absolute errors were below this value

## How variable is this score?

Paired 95.0% percentile intervals from 1000 bootstrap draws over 48
observations. Negative differences favor the primary model.

| quantity   | estimate | lower  | upper  |
|------------|----------|--------|--------|
| primary    | 0.994    | 0.773  | 1.212  |
| baseline   | 2.969    | 2.447  | 3.468  |
| difference | -1.975   | -2.563 | -1.383 |

Paired percentile intervals conditional on the fitted models; negative
differences favor the primary model. These intervals omit fitting and
selection uncertainty and assume independent sampling units.

Model comparison

## What trade-offs did the candidates make?

AutoXplainR compares the supplied candidates on two visible dimensions
rather than hiding judgment inside one weighted score. A
Pareto-efficient model is not beaten by another supplied model on both
test-set performance and the displayed resource proxy.

Compared models

**4**Primary, candidates, and simple baseline

Pareto-efficient

**3**Not dominated on both displayed dimensions

Best observed score

**linear regression**RMSE on these test-set rows

Lowest trade-off proxy

**small decision tree**approximate model-object size (KB); resource
proxy

![](data:image/svg+xml;base64,PHN2ZyBjbGFzcz0idHJhZGVvZmYtcGxvdCIgdmlld2JveD0iMCAwIDcyMCAzNjAiIHJvbGU9ImltZyIgYXJpYS1sYWJlbD0iTW9kZWwgdHJhZGUtb2ZmIGNoYXJ0IGNvbXBhcmluZyBSTVNFIGFuZCBhcHByb3hpbWF0ZSBtb2RlbC1vYmplY3Qgc2l6ZSAoS0IpIGZvciA0IG1vZGVscy4iPjxsaW5lIHgxPSI3NiIgeTE9IjI5MiIgeDI9IjY4NCIgeTI9IjI5MiIgY2xhc3M9ImNoYXJ0LWF4aXMiPjwvbGluZT48bGluZSB4MT0iNzYiIHkxPSIzNCIgeDI9Ijc2IiB5Mj0iMjkyIiBjbGFzcz0iY2hhcnQtYXhpcyI+PC9saW5lPjx0ZXh0IHg9IjM4MCIgeT0iMzQyIiBjbGFzcz0iYXhpcy1sYWJlbCI+bG93ZXIgYXBwcm94aW1hdGUgbW9kZWwtb2JqZWN0IHNpemUgKEtCKSDihpI8L3RleHQ+PHRleHQgeD0iMTgiIHk9IjE2MyIgdHJhbnNmb3JtPSJyb3RhdGUoLTkwIDE4IDE2MykiIGNsYXNzPSJheGlzLWxhYmVsIj5iZXR0ZXIgUk1TRSDihpI8L3RleHQ+PHBvbHlsaW5lIHBvaW50cz0iNzYsODguNTcgMjU4LjQ3LDYzLjQ5IDY4NCwzNCIgY2xhc3M9InBhcmV0by1saW5lIj48L3BvbHlsaW5lPjxnPjxjaXJjbGUgY3g9IjY4NCIgY3k9IjM0IiByPSIxMCIgY2xhc3M9InRyYWRlb2ZmLXBvaW50IHRyYWRlb2ZmLXByaW1hcnkgdHJhZGVvZmYtcGFyZXRvIj48L2NpcmNsZT48dGV4dCB4PSI2NzEiIHk9IjU4IiB0ZXh0LWFuY2hvcj0iZW5kIiBjbGFzcz0icG9pbnQtbGFiZWwiPmxpbmVhciByZWdyZXNzaW9uPC90ZXh0PjwvZz48Zz48Y2lyY2xlIGN4PSIyNTguNDciIGN5PSI2My40OSIgcj0iMTAiIGNsYXNzPSJ0cmFkZW9mZi1wb2ludCB0cmFkZW9mZi1jYW5kaWRhdGUgdHJhZGVvZmYtcGFyZXRvIj48L2NpcmNsZT48dGV4dCB4PSIyNzEuNDciIHk9IjUyLjQ5IiB0ZXh0LWFuY2hvcj0ic3RhcnQiIGNsYXNzPSJwb2ludC1sYWJlbCI+ZmxleGlibGUgZGVjaXNpb24gdHJlZTwvdGV4dD48L2c+PGc+PGNpcmNsZSBjeD0iNzYiIGN5PSI4OC41NyIgcj0iMTAiIGNsYXNzPSJ0cmFkZW9mZi1wb2ludCB0cmFkZW9mZi1jYW5kaWRhdGUgdHJhZGVvZmYtcGFyZXRvIj48L2NpcmNsZT48dGV4dCB4PSI4OSIgeT0iNzcuNTciIHRleHQtYW5jaG9yPSJzdGFydCIgY2xhc3M9InBvaW50LWxhYmVsIj5zbWFsbCBkZWNpc2lvbiB0cmVlPC90ZXh0PjwvZz48Zz48Y2lyY2xlIGN4PSIzNjQuMiIgY3k9IjI5MiIgcj0iOCIgY2xhc3M9InRyYWRlb2ZmLXBvaW50IHRyYWRlb2ZmLWJhc2VsaW5lIj48L2NpcmNsZT48dGV4dCB4PSIzNzcuMiIgeT0iMjgxIiB0ZXh0LWFuY2hvcj0ic3RhcnQiIGNsYXNzPSJwb2ludC1sYWJlbCI+aW50ZXJjZXB0LW9ubHkgYmFzZWxpbmU8L3RleHQ+PC9nPjwvc3ZnPg==)

### How to read this

1.  Up means better test-set performance.
2.  Left means lower approximate model-object size (KB), used here as a
    resource proxy.
3.  Outlined points form the supplied Pareto frontier.

**The primary model remains pre-specified.** Candidate ranks are
descriptive; selecting a winner on these same evaluation rows and
quoting its score as final performance would be optimistic.

| Model | Role | RMSE | approximate model-object size (KB) | Pareto-efficient |
|----|----|----|----|----|
| linear regression | primary | 0.994 | 67.3125 | yes |
| flexible decision tree | candidate | 1.2197 | 53.3203 | yes |
| small decision tree | candidate | 1.4117 | 47.3203 | yes |
| intercept-only baseline | baseline | 2.9691 | 56.7969 | no |

### How are these model families different?

**Prior/model-capacity knowledge:** the nonlinearity and interaction
columns are reviewed behavior cards. They say what each family can
represent; they do not show that this fitted model actually used those
patterns.

| Model | Family | Backend | Capacity: nonlinearity | Capacity: interactions | Computed RMSE |
|----|----|----|----|----|----|
| linear regression | linear | stats | none unless encoded in features | none unless specified in features | 0.994 |
| flexible decision tree | tree | rpart | stepwise | automatic along tree paths | 1.2197 |
| small decision tree | tree | rpart | stepwise | automatic along tree paths | 1.4117 |

**Computed evidence from this analysis:** evaluation performance and
paired prediction disagreement are calculated on common evaluation rows.
Repeated permutation feature importance in the Patterns section is also
computed evidence of model reliance, not a property guaranteed by the
family card.

- main_model has the best supplied rmse score (0.994).
- main_model and small_tree differ most on average (0.988 using absolute
  difference in predicted target units).

### Where did supplied model choices disagree?

The same evaluation rows were scored by every supplied non-baseline
candidate. A large gap means the answer depends on model specification,
even when the data row is unchanged.

Compared candidates

**3**Simple baseline excluded

Median prediction range

**1.2401**In outcome units

90th-percentile range

**2.3727**Nine in ten rows were below this

Largest prediction range

**2.8461**Most specification-sensitive row

| Row | Observed | Lowest prediction | Highest prediction | Range  |
|-----|----------|-------------------|--------------------|--------|
| 38  | 0.0506   | -0.9611           | 1.8851             | 2.8461 |
| 31  | -1.4429  | -3.9355           | -1.1328            | 2.8027 |
| 47  | 5.857    | 5.1801            | 7.9733             | 2.7933 |
| 44  | 3.5613   | 1.8851            | 4.6228             | 2.7377 |
| 25  | -4.4208  | -3.6057           | -1.1328            | 2.4728 |

**Disagreement is a review signal, not an error bar.** The compared
candidates can have very different evaluation performance; read this
beside the score table above. It does not identify the correct
prediction or provide uncertainty coverage.

**Trade-off boundary:** Approximate in-memory R object size (or
engine-reported H2O size) and runtime measure operational resource use,
not structural complexity. Pareto status compares only the supplied
models on the supplied evaluation data and exact displayed axes.
Resource proxies are not structural complexity; this is not a final
model-selection rule.

How the model works

## Patterns used for prediction

Permutation importance asks how much evaluation performance worsens when
one input is shuffled. It describes model reliance, not cause and
effect.

| Input | Reliance score | Repeat consistency | How to communicate it    |
|-------|----------------|--------------------|--------------------------|
| x     | 2.7724         | 100.0%             | stable marginal evidence |
| z     | 0.3196         | 100.0%             | stable marginal evidence |

Reliance scores are changes in the selected performance metric after
shuffling. Repeat consistency is computational stability, not population
confidence.

### What direction did the fitted pattern take?

These curves change one input within observed support and summarize the
model's prediction. They are descriptions of the fitted model, not
intervention effects.

ALE fitted effect

### x

Within the evaluated range, the model's predicted value generally
increased. The displayed effect spans 10.5633 prediction units.

![](data:image/svg+xml;base64,PHN2ZyBjbGFzcz0iZWZmZWN0LXBsb3QiIHZpZXdib3g9IjAgMCA1MjAgMTcwIiByb2xlPSJpbWciIGFyaWEtbGFiZWw9IkFMRSBjdXJ2ZSBmb3IgeCByYW5naW5nIGZyb20gLTQuMjA1IHRvIDYuMzU5Ij48bGluZSB4MT0iMjIiIHkxPSIxNDgiIHgyPSI0OTgiIHkyPSIxNDgiIGNsYXNzPSJheGlzIj48L2xpbmU+PHBvbHlsaW5lIHBvaW50cz0iMjIuMDAsMTQ4LjAwIDUzLjczLDEzNS45MiA4NS40NywxMjkuMjEgMTE3LjIwLDEyMi4zNiAxNDguOTMsMTE3LjUzIDE4MC42NywxMTIuODYgMjEyLjQwLDEwOC4zMiAyNDQuMTMsMTA0LjYxIDI3NS44Nyw5NS4yMSAzMDcuNjAsOTMuMjUgMzM5LjMzLDg3LjI5IDM3MS4wNyw4My4yNiA0MDIuODAsNzkuMDQgNDM0LjUzLDY4Ljc1IDQ2Ni4yNyw1Ny45MyA0OTguMDAsMjIuMDAiIGNsYXNzPSJlZmZlY3QtbGluZSI+PC9wb2x5bGluZT48L3N2Zz4=)

ALE fitted effect

### z

Within the evaluated range, the model's predicted value generally
increased. The displayed effect spans 2.9417 prediction units.

![](data:image/svg+xml;base64,PHN2ZyBjbGFzcz0iZWZmZWN0LXBsb3QiIHZpZXdib3g9IjAgMCA1MjAgMTcwIiByb2xlPSJpbWciIGFyaWEtbGFiZWw9IkFMRSBjdXJ2ZSBmb3IgeiByYW5naW5nIGZyb20gLTEuMjA5IHRvIDEuNzMzIj48bGluZSB4MT0iMjIiIHkxPSIxNDgiIHgyPSI0OTgiIHkyPSIxNDgiIGNsYXNzPSJheGlzIj48L2xpbmU+PHBvbHlsaW5lIHBvaW50cz0iMjIuMDAsMTQ4LjAwIDUzLjczLDEzNC45MSA4NS40NywxMzAuNzIgMTE3LjIwLDEyMi41NSAxNDguOTMsMTEzLjMwIDE4MC42NywxMDguNjUgMjEyLjQwLDEwNC40NSAyNDQuMTMsMTAwLjg5IDI3NS44Nyw5NS43NyAzMDcuNjAsOTAuMjMgMzM5LjMzLDg1LjMyIDM3MS4wNyw4NC40NCA0MDIuODAsNzkuNzUgNDM0LjUzLDcwLjg4IDQ2Ni4yNyw0Ny43MCA0OTguMDAsMjIuMDAiIGNsYXNzPSJlZmZlY3QtbGluZSI+PC9wb2x5bGluZT48L3N2Zz4=)

How cautious should I be?

## Explanation reliability

A

**This grade is a triage aid, not a certification.** Heuristic evidence
grade; not a certification or formal inferential guarantee.

Repeat-stable claims

**62.5%**Feature statements graded A or B

Competitive models

**1 / 4**Within the configured performance tolerance

Rank agreement

**n/a**Agreement among competitive supplied models

Largest association

**0.115**High values pressure marginal explanations

### Warnings and next actions

warning`limited_importance_evidence`

### 3 model-feature claim(s) are qualified or unsupported.

**Evidence:** 3 interval(s) include zero; 0 have sign stability below
0.8; 0 exceed the dependence threshold.

**Next action:** Inspect repeat distributions and dependence, increase
evaluation data when uncertainty is material, and avoid ranking
unsupported features.

Open the technical evidence audit

### Supplied models

| model           | score   | metric | near_optimal | relative_gap |
|-----------------|---------|--------|--------------|--------------|
| main_model      | 0.99398 | rmse   | yes          | 0.00         |
| simple_baseline | 2.96915 | rmse   | no           | 1.98714      |
| small_tree      | 1.41171 | rmse   | no           | 0.42027      |
| flexible_tree   | 1.21974 | rmse   | no           | 0.22713      |

### Repeat-level importance evidence

### main_model

| Feature | Relative magnitude | Importance | MC interval | Grade | Permitted claim |
|----|----|----|----|----|----|
| x |  | 2.7724 | \[2.6482, 2.8966\] | A | stable marginal evidence |
| z |  | 0.3196 | \[0.2801, 0.3592\] | A | stable marginal evidence |

### simple_baseline

| Feature | Relative magnitude | Importance | MC interval | Grade | Permitted claim |
|----|----|----|----|----|----|
| x |  | 0.00 | \[0.00, 0.00\] | C | sensitivity finding only |
| z |  | 0.00 | \[0.00, 0.00\] | C | sensitivity finding only |

### small_tree

| Feature | Relative magnitude | Importance | MC interval | Grade | Permitted claim |
|----|----|----|----|----|----|
| x |  | 2.158 | \[2.0402, 2.2757\] | A | stable marginal evidence |
| z |  | 0.00 | \[0.00, 0.00\] | C | sensitivity finding only |

### flexible_tree

| Feature | Relative magnitude | Importance | MC interval | Grade | Permitted claim |
|----|----|----|----|----|----|
| x |  | 2.6027 | \[2.5319, 2.6735\] | A | stable marginal evidence |
| z |  | 0.2199 | \[0.1754, 0.2644\] | A | stable marginal evidence |

### Feature association diagnostics

| feature | max_association | associated_feature | high_dependence |
|---------|-----------------|--------------------|-----------------|
| x       | 0.115           | z                  | no              |
| z       | 0.115           | x                  | no              |

Permutation intervals describe random shuffling variation, not
uncertainty about a population. Near-optimal status is relative only to
models supplied here.

Interpretation boundaries

## What this analysis does not establish

### Reasonable statements

- How the fitted model performed on this evaluation set
- Which inputs the model relied on under the configured shuffling test
- Which fitted prediction patterns appeared in the evaluation data

### Statements requiring other evidence

- That changing an input will cause the predicted outcome to change
- That performance will transfer to another population or future period
- That the model is fair, safe, or suitable for deployment

Reproducibility

## How this result was produced

- Generated: 2026-09-07 09:40:54 UTC
- Package version: 0.3.0
- Engine: base
- Target: outcome
- Task: regression
- Training rows: 192
- Evaluation rows: 48
- Evaluation role: test
- Split method: reproducible random holdout
- Primary model ID: main_model
- Primary model label: linear regression
- Permutation repeats: 20
- Seed: 123
- Explainer IDs: axr-76d2bc08, axr-05146883, axr-344e90a9, axr-3d98d60d
