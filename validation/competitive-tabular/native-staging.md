# Separate native training from locked scoring

Declared on 12 September 2026, before any full-training native reference fit
or locked evaluation score was run.

Native reference training may finish before the final package candidate is
frozen. This permits useful overlap with report development while keeping the
evaluation outcomes closed. It does not change the reference algorithm,
calibration split, two settings, seeds, thread counts or full training rows.

The explicit `fit-only` stage opens only the declared training RDS. It performs
the original two training-calibration fits, chooses by calibration loss and
refits the selected setting on every training row. It saves the native model,
calibration predictions, parameters, versions, source hashes, process time and
memory use. It must not open either evaluation RDS or calculate evaluation
metrics. A fit-only completion establishes no acceptance quality result.

A separate `score-only` stage requires the final acceptance freeze manifest.
That manifest binds the exact completed fit's process record and model hash,
as well as the original partitions and every protocol amendment. Only then may
the scoring process open evaluation features and outcomes. It writes complete
predictions and independent metrics, followed by fresh-session prediction
replay. Every earlier fit failure or timeout remains in the evidence.

The original full-reference limit remains 7,200 seconds for the combined fit
and scoring processes. Scoring receives only the remaining time. Both stages
have the same 24-GiB address-space limit and four native threads; report their
individual times and the combined total, and the larger observed peak RSS.
Development staging checks use the corresponding combined 1,200-second limit.

Full-training fits will not start while the candidate-v2 development fits are
running. Later overlap must be recorded as shared-host work. The full native
forest completion and quality requirements remain mandatory; separating stages
does not relax a resource failure or substitute calibration scores for final
evaluation scores.
