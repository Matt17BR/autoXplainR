# AutoXplainR Product Contract

## North star

AutoXplainR helps a person fitting one of their first tabular models answer four
questions without needing to assemble a modeling stack:

1. What kind of prediction problem do I have?
2. How well does the fitted model work on data it did not train on?
3. What patterns does the model use, and how reliable are those explanations?
4. How can I communicate the result without overstating it?

The package should make the safe path the easy path. An explanation evidence
audit is an internal quality layer and an advanced view; it is not the first
concept a new user must understand.

## Default user journey

The primary workflow must accept a data frame and target name, then:

- detect regression, binary classification, or multiclass classification;
- create a reproducible held-out evaluation split when one is not supplied;
- fit a simple baseline and an understandable primary model without requiring
  Java or a cloud account;
- compare them using task-appropriate metrics with plain-language definitions;
- compute global importance and feature effects with limitations attached;
- produce one self-contained, accessible report;
- optionally add an LLM narrative that is subordinate to computed results.

The next step remains one explicit argument away: `model_set = "comparison"`
adds a small, local candidate set and displays performance-versus-complexity
trade-offs without silently changing the pre-specified primary model. Advanced
users may supply their own split, model, prediction function, H2O AutoML run,
explanation configuration, or narrative provider.

## Progressive disclosure

The same result should support three levels of detail:

1. **Guided:** what was fitted, whether it beats a simple baseline, the most
   important patterns, probability calibration, binary decision-threshold
   trade-offs, missingness context, and concrete cautions.
2. **Compare:** visible candidate performance, complexity, Pareto status, and
   case-level prediction disagreement, with the warning that held-out ranks are
   descriptive rather than a free tuning loop.
3. **Evidence:** permutation variability, dependence, supplied-model
   multiplicity, ALE/PDP support, and provenance.
4. **Developer:** repeat-level values, prediction contracts, recipes, metrics,
   prompts, and machine-readable configuration.

Every technical term in the guided report needs an adjacent definition. Grades
and warnings must explain what action the user should take next.

## Narrative provider policy

Narratives never receive raw rows, fitted model objects, secrets, or case-level
predictions. Remote use is explicit; the package never discovers a key and
sends data merely because the key exists.

Provider order reflects the July 2026 public offerings and must remain
configurable because model availability changes:

| Provider | Role | Default model | Trade-off |
|---|---|---|---|
| Local template | Default | none | Reproducible and private; less fluent |
| Google Gemini | Recommended hosted free tier | `gemini-3.5-flash` | Strong prose and explicit free tier; data leaves the machine |
| Groq | Fast hosted alternative | `openai/gpt-oss-120b` | Strict schema output on a free plan; model catalog can change |
| Cloudflare Workers AI | Alternative hosted free allowance | `@cf/openai/gpt-oss-120b` | OpenAI-compatible endpoint; needs a token and account ID |
| Ollama | Local generative option | `gemma3:4b` | Private and no API cost; requires a local runtime/model |
| OpenRouter | Experimental free router | `openrouter/free` | Broad access; selected model and availability are not reproducible |

All provider adapters must share one prompt contract, expose the provider and
resolved model in provenance, time out, handle rate limits, and fall back to
the deterministic report without losing the analysis.

Schema-capable providers must return a small validated object rather than final
report markup. AutoXplainR renders those fields and adds fixed interpretation
boundaries locally. Invalid or incomplete structured output triggers the same
transparent deterministic fallback. Schema validity is a format guarantee, not
evidence that generated claims are true.

Gemini uses the stateless Interactions API with `store = false`; the package
does not need provider-side conversation state for a one-shot aggregate memo.
OpenRouter routing must require schema support when a schema is requested.

Hosted-provider recommendations must be checked against official model,
pricing, rate-limit, structured-output, and privacy documentation before every
release. `LLM_PROVIDERS.md` is the dated decision record; the package must not
describe a hosted plan as permanently free.

## Non-goals

- AutoXplainR does not automate causal claims, fairness certification, or
  deployment approval.
- It does not hide the train/evaluation split or select metrics without
  explaining them.
- It does not require an LLM to produce a complete report.
- It does not make users understand Rashomon sets before they can evaluate a
  first model.

## Release acceptance

A release is not ready unless a first-time user can complete the primary
workflow from the README, understand the first screen of the report, and obtain
the same numerical conclusions with remote narrative generation disabled.
