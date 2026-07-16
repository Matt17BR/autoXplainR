# AutoXplainR Product Contract

## North star

AutoXplainR helps a person fitting one of their first tabular models
answer five questions without needing to assemble a modeling stack:

1.  What kind of prediction problem do I have?
2.  Should I fit an understandable reference or automatically tune
    alternatives?
3.  How well does the selected model work on data it did not train on?
4.  What patterns does the model use, and how reliable are those
    explanations?
5.  How can I communicate the result without overstating it?

The package should make the safe path the easy path. An explanation
evidence audit is an internal quality layer and an advanced view; it is
not the first concept a new user must understand.

## Default user journey

The primary workflow must accept a data frame and target name, then:

- detect regression, binary classification, or multiclass
  classification;
- create a reproducible held-out evaluation split when one is not
  supplied;
- fit a simple baseline and an understandable primary model without
  requiring Java or a cloud account;
- compare them using task-appropriate metrics with plain-language
  definitions;
- compute global importance and feature effects with limitations
  attached;
- produce one self-contained, accessible report;
- optionally add an LLM narrative that is subordinate to computed
  results.

Two next steps remain one explicit argument away. `model_set = "tuned"`
uses training-only resampling to compare a named portfolio spanning
linear, regularized, additive, tree, forest, boosting, neural, kernel,
neighbor, and MARS families before opening the outer holdout.
`model_set = "comparison"` adds a small, pre-specified local candidate
set and displays performance-versus-resource trade-offs without silently
changing the primary model. Advanced users may supply their own split,
model, prediction function, H2O AutoML run, explanation configuration,
or narrative provider.

User-supplied evaluation rows are neutral by default. A caller may
assert a test or validation role when the study design supports it; the
package must not infer independence from an argument name. Exact
duplicate-valued records across training and evaluation data are a
possible-leakage diagnostic, not proof that the observational units
overlap.

## Tuning contract

Local automatic tuning must remain comprehensible enough for a first
model:

- the final evaluation set cannot rank configurations, tune thresholds,
  learn preprocessing, or trigger another search;
- preprocessing is refitted within every training resample;
- the recommended search covers linear, regularized, additive where
  supported, decision-tree, random-forest, and gradient-boosted-tree
  families; an extended portfolio adds neural, kernel, neighbor, and
  MARS alternatives;
- a requested portfolio is fixed before resampling and never changes
  according to which optional packages happen to be installed;
- the candidate settings, fold scores, failures, timing, selection rule,
  and selected hyperparameters remain inspectable;
- the default one-standard-error rule uses a reviewed family priority
  and then a family-specific flexibility proxy; it never compares unlike
  raw complexity units, while an explicit `"best"` rule remains
  available; and
- reports distinguish the training-resampled selection score from the
  final held-out evaluation score in adjacent plain language.

The beginner presets have fixed meanings: `core` is the dependency-light
linear/tree/neural tournament; `recommended` compares linear,
regularized, additive where supported, tree, forest, and boosting
families; and `extended` adds neural, kernel, neighbor, and MARS
families. Task-incompatible families may be excluded before tuning, but
installed-package state must never redefine a requested portfolio.

[`tuning_control()`](https://matt17br.github.io/autoXplainR/reference/tuning_control.md)
is the advanced escape hatch, not part of the first screen. It may
validate custom per-family grids and exact budgets, an alternate
supported loss, ordinary supplied V-fold IDs, out-of-fold retention, and
a failure policy. Supplied folds require an explicit outer test set and
must be described honestly: ordinary V-fold IDs are not grouped
resampling, forward-chaining, or rolling-origin validation unless the
user constructs a design with the required scientific meaning outside
the package.

H2O remains an optional Java-backed AutoML engine for broader searches
and stacked ensembles. It shares the result and explanation contracts,
but it is not required for the local portfolio. Its engine leaderboard
remains separate from the common outer-evaluation leaderboard: H2O
cross-validation, or an explicitly supplied validation frame when
`nfolds = 0`, selects the primary model. The latter compares that fixed
choice, alternative fitted models, and a simple baseline on aligned rows
without re-selecting from the outer scores. By default, external
evaluation rows remain outside H2O validation as well as fitting. Seeded
H2O runs must disclose that wall-clock limits and eligible Deep Learning
models weaken reproducibility.

## Cross-model explanation contract

Every retained family must be comparable through the same prediction and
evaluation interfaces. The package must keep three statements distinct:

1.  a behavior card is reviewed prior knowledge about what a family can
    represent;
2.  held-out scores, same-row prediction gaps, and permutation reliance
    are computed from the supplied analysis; and
3.  aligned ALE or PDP curves describe how fitted prediction functions
    differ for one named input over common supported values.

[`compare_model_behavior()`](https://matt17br.github.io/autoXplainR/reference/compare_model_behavior.md)
serves the portfolio-level question;
[`compare_model_effects()`](https://matt17br.github.io/autoXplainR/reference/compare_model_effects.md)
serves the feature-level question. Neither family capacity nor a fitted
curve establishes that an interaction was used, that an input caused an
outcome, or that the result transfers to another population.

## Progressive disclosure

The same result should support five levels of detail:

1.  **Guided:** what was fitted, whether it beats a simple baseline, the
    most important patterns, probability calibration, binary
    decision-threshold trade-offs, missingness context, and concrete
    cautions.
2.  **Tune:** model families, hyperparameter settings, training-only
    fold scores, selection rule, and the boundary between tuning and
    final evaluation.
3.  **Compare:** visible candidate performance, explicitly labeled
    resource or structural proxies, Pareto status, case-level prediction
    disagreement, and optional aligned feature curves, with the warning
    that held-out ranks are descriptive rather than a free tuning loop.
4.  **Evidence:** permutation variability, dependence, supplied-model
    multiplicity, ALE/PDP support, and provenance.
5.  **Developer:** repeat-level values, prediction contracts, recipes,
    metrics, prompts, and machine-readable configuration.

Every technical term in the guided report needs an adjacent definition.
Grades and warnings must explain what action the user should take next.

## Narrative provider policy

Narratives never receive raw rows, fitted model objects, secrets, or
case-level predictions. Remote use is explicit; the package never
discovers a key and sends data merely because the key exists.

Provider order reflects the July 2026 public offerings and must remain
configurable because model availability changes:

| Provider | Role | Default model | Trade-off |
|----|----|----|----|
| Local template | Default | none | Reproducible and private; less fluent |
| Google Gemini | Recommended hosted free tier | `gemini-3.5-flash` | Strong prose and explicit free tier; data leaves the machine |
| Groq | Fast hosted alternative | `openai/gpt-oss-120b` | Strict schema output on a free plan; model catalog can change |
| Cloudflare Workers AI | Alternative hosted free allowance | `@cf/openai/gpt-oss-120b` | OpenAI-compatible endpoint; needs a token and account ID |
| Ollama | Local generative option | `gemma3:4b` | Private and no API cost; requires a local runtime/model |
| OpenRouter | Experimental free router | `openrouter/free` | Broad access; selected model and availability are not reproducible |

All provider adapters must share one prompt contract, expose the
provider and resolved model in provenance, time out, handle rate limits,
and fall back to the deterministic report without losing the analysis.

Schema-capable providers must return a small validated object rather
than final report markup. AutoXplainR renders those fields and adds
fixed interpretation boundaries locally. Invalid or incomplete
structured output triggers the same transparent deterministic fallback.
Schema validity is a format guarantee, not evidence that generated
claims are true.

Gemini uses the stateless Interactions API with `store = false`; the
package does not need provider-side conversation state for a one-shot
aggregate memo. OpenRouter routing must require schema support when a
schema is requested.

Hosted-provider recommendations must be checked against official model,
pricing, rate-limit, structured-output, and privacy documentation before
every release. `LLM_PROVIDERS.md` is the dated decision record; the
package must not describe a hosted plan as permanently free.

## Non-goals

- AutoXplainR does not automate causal claims, fairness certification,
  or deployment approval.
- It does not hide the train/evaluation split or select metrics without
  explaining them.
- It does not require an LLM to produce a complete report.
- It does not make users understand Rashomon sets before they can
  evaluate a first model.

## Release acceptance

A release is not ready unless a first-time user can complete the primary
workflow from the README, understand the first screen of the report, and
obtain the same numerical conclusions with remote narrative generation
disabled.
