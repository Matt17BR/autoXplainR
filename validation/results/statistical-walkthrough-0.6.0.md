# Statistical expert walkthrough of the final 0.6.0 candidate

This is an independent agent review of the actual generated interface, not a
participant study or evidence of comparative user benefit. The reviewer first
read visible DOM text and screenshots at 1440 and 390 px, recorded the answers,
and then checked the retained RDS and benchmark oracle. Source baseline is
`12e9ba1d5b0aca07c6477d0055d45b5795c68114` with the uncommitted 0.6.0 overhaul.
The reports came from the final canonical generation on 2026-09-07.

## Tasks and observed answers

| Expert task | Answer obtained from the report | Independent check and scope |
|---|---|---|
| Explain why the multiclass primary differs from the lowest-loss setting. | The six-unit network with decay 0.01 has CV log loss 0.0408. The selected and final fit both use two units and decay 0.03, with loss 0.054. The stated policy permits loss up to 0.0619 and prefers the smallest recorded connection-count proxy within an eligible family. | Retained best loss 0.0408408844 plus its own SE 0.02108734 gives threshold 0.06192822. The selected loss is 0.0540494483. The page explicitly limits family preference and capacity proxies; this is not a statistical equivalence claim or proof that a decay value is optimal. |
| Distinguish a successful alternative from an eligible primary. | Neural has 5 settings within the limit out of 7 successful settings. Trees have 0 within the limit out of 7 successful settings; the best tree is labeled a retained family alternative. Multinomial logistic has 0 successful settings and 1 failed setting. | Candidate records contain five eligible neural IDs and seven successful tree IDs. Tree CV best is 0.69653822. The revised “Within limit / successful” header preserves this distinction rather than implying that the tree family is invalid. |
| Investigate the omitted multinomial family. | Selecting its family and opening its setting reveals iteration-limit termination in all five folds, exclusion policy, no valid CV score, and the actual backend default of 100 iterations. | All five retained fold records have `not_converged`; learned controls retain `maxit = 100`, decay 0 and convergence code 1. This is optimizer failure, not evidence that every possible multinomial specification performs poorly. |
| Inspect the chosen network's evidence and limits. | The chosen network has five 96-row training / 24-row validation folds, zero omitted rows and five converged statuses. Requested/effective size is 2, decay is 0.03; learned controls show 4 encoded inputs, 19 fitted weights and maxit 500. | Independently weighting the five fold losses by their validation sizes gives 0.05404944831395. Fold losses range from 0.02179884 to 0.12548197. The page calls the spread descriptive and warns that overlapping training folds do not supply independent-test confidence intervals. |
| Identify what was searched and a defensible next action. | Seven of twelve available neural tuples were scheduled; five were untested. The winning setting is interior to the scheduled ranges. The report offers the exact scheduled custom grid, budgets, metric, optimizer policy and family order. | The command is explicitly a grid reproduction, not an identical replay without the original data, preprocessing, validation and seed. Joint tuples do not identify individual parameter effects, and the text does not claim that an interior winner proves adequate coverage. |
| Interpret the supplied-model timing comparison. | On a common 32-row batch, displayed medians are 0.009665 ms/row for the current logistic model, 0.0142 for the tree and 0.005919 for the historical rate. All three have 3/3 completed repetitions. | The logistic median block is approximately 30 ms for 97 batch calls: `30 / 97 / 32 = 0.00966495` ms/row. Independently calculated quartiles and all per-row medians match the displayed significant digits and cost-axis values. These are machine/process/batch observations, not single-request latency or a deployment ranking. |
| Understand timing reliability and privacy. | The protocol reports an observed 1 ms clock step, a two-second timed budget and seed 6102. It distinguishes guided preprocessing outside the call from custom-function transformations inside it. The summary export contains no individual observations. | Raw measured blocks meet the recorded 20-step calibration guard. Quartiles are labeled descriptive, not confidence intervals. The supplied gate verifies absent row payloads, absent private sampling indices, warning/status retention, and actual row-link behavior in the separate opt-in row export. |

## Friction observed

- The primary-selection answer is available before the long evidence details.
  Actual parameter labels and a default view of the final primary's family make
  the decision inspectable without first decoding configuration IDs.
- On a 390 px display, the five-column benchmark table uses a 606 px-wide table
  inside a 360 px scroll region. Model names and median costs are visible first;
  quartiles and repeat counts require horizontal scrolling. The region is
  keyboard-focusable (`role="region"`, `tabindex="0"`). This remains a mobile
  reading cost, even though the page itself does not overflow.
- The initial walkthrough found implementation-style fold column labels and
  empty warning/error columns. A bounded follow-up changed these to readable
  headings, omitted empty issue columns, and replaced an all-zero omitted-row
  column with an explicit no-omissions sentence. The successful neural example
  now has six columns. Nonzero omissions, errors and warnings remain visible;
  the underlying R records are unchanged. Long per-fold parameter details and
  horizontal scrolling for advanced mobile tables remain a reading cost.
- The follow-up also found that selection printing expanded every family and
  candidate. It now preserves the selected family and existing open candidate,
  and restores that state after printing. The actual multiclass PDF contains
  the chosen neural rationale and `neural_02` fold evidence, with no other
  family's rationale or unopened fold records. The reviewed PDF has three pages.
- The small three-repeat benchmark fixture establishes arithmetic and reporting
  consistency. It does not establish stable speed differences under other batch
  sizes, machine load, prediction adapters or deployment conditions.

No new arithmetic, selection-identity or benchmark-integrity blocker was found
in these inspected states. This is a bounded review, not an exhaustive claim.

## Executed gates and local evidence

- Selection: **107 checks passed**, including independent fold/pooled positions,
  exact threshold, candidate identity, 1440/390/320 px geometry, no-JavaScript
  availability, actual PDF chart text of at least 8 pt, chosen-family/open-detail
  print scope and preserved state after returning from print. The presentation
  test suite passed 115 assertions, including preserved adverse evidence.
- Supplied models: **166 checks passed**, including raw-block arithmetic,
  independently calculated quartiles, displayed costs/axes, reference exclusion,
  exported-row navigation, all 101 literal decimal cutoffs, privacy and actual
  PDF text of at least 8 pt. Poppler printed named-destination warnings; numeric
  and font-size checks still completed successfully.

The local evidence root is
`/home/mmazzarelli/.cache/autoxplain-overhaul-0.6/`:

- `final-selection/selection-checks.json`, screenshots and `selection.pdf`;
- `final-supplied/report-costs-checks.json`, screenshots and `benchmark.pdf`;
- `expert-statistical/observed-before-oracles.json`, `detail-observations.json`,
  `fold-arithmetic.txt`, `multiclass-oracle-check.txt` and the reviewed screenshots;
- `expert-statistical/selection-repair-review.json`, repaired fold screenshots
  and `multiclass-current-selection.pdf` record the bounded follow-up.

The durable runners are `validation/check-selection.py` and
`validation/check-supplied-models.py`, invoked with the canonical
`selection-cases` and `cases` directories respectively. The follow-up changed
only presentation and validation files. Its synthetic selection report and
multiclass report were refreshed from the existing evidence without refitting.
