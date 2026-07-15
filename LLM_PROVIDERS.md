# Narrative provider guide

AutoXplainR does not need a language model to fit, evaluate, explain, or report
a model. The default `provider = "local"` is deterministic, private, and works
offline. A generative provider is an optional communication layer over
aggregated evidence that AutoXplainR has already computed.

The defaults below were checked against official provider documentation on
2026-07-15. Limits, models, prices, and data-use terms can change independently
of this package, so inspect `narrative_providers()` and the linked provider page
before relying on a hosted service.

| Provider | Default | Free access | Structured output | Main trade-off |
|---|---|---|---|---|
| Local template | none | No API cost | Not needed | Most reproducible and private; intentionally less conversational |
| [Google Gemini](https://ai.google.dev/gemini-api/docs/models/gemini-3.5-flash) | `gemini-3.5-flash` | [Hosted free tier](https://ai.google.dev/gemini-api/docs/pricing) | [JSON schema](https://ai.google.dev/gemini-api/docs/structured-output) | Recommended hosted default; free-tier prompts may be used to improve Google products |
| [Groq](https://console.groq.com/docs/model/openai/gpt-oss-120b) | `openai/gpt-oss-120b` | [Rate-limited free plan](https://console.groq.com/docs/rate-limits) | [Strict JSON schema](https://console.groq.com/docs/structured-outputs) | Fast and schema-capable; model catalog and limits can change |
| [Cloudflare Workers AI](https://developers.cloudflare.com/workers-ai/models/gpt-oss-120b/) | `@cf/openai/gpt-oss-120b` | [10,000 neurons per day](https://developers.cloudflare.com/workers-ai/platform/pricing/) | Not documented for this default model | Useful alternative hosted endpoint; needs both an API token and account ID |
| [Ollama](https://docs.ollama.com/api/openai-compatibility) | `gemma3:4b` | Local compute | [JSON schema](https://ollama.com/blog/structured-outputs) | Data stays local; installation, model download, memory, and speed are the user's responsibility |
| [OpenRouter](https://openrouter.ai/openrouter/free/providers) | `openrouter/free` | [Limited free-model requests](https://openrouter.ai/docs/faq) | [Provider-dependent schema](https://openrouter.ai/docs/guides/features/structured-outputs) | Convenient experimentation; the selected free model and availability are not reproducible |

## Setup

Never put a key in a script, report, issue, or committed `.Renviron` file. Set
it in the current session while experimenting, or in a user-level environment
file outside the project.

```r
# Recommended hosted free-tier path
Sys.setenv(GEMINI_API_KEY = "...")
memo <- generate_natural_language_report(result, provider = "gemini")

# Fast hosted alternative
Sys.setenv(GROQ_API_KEY = "...")
memo <- generate_natural_language_report(result, provider = "groq")

# Cloudflare requires two values
Sys.setenv(
  CLOUDFLARE_API_TOKEN = "...",
  CLOUDFLARE_ACCOUNT_ID = "..."
)
memo <- generate_natural_language_report(result, provider = "cloudflare")

# Local generative model; first run `ollama pull gemma3:4b`
memo <- generate_natural_language_report(result, provider = "ollama")
```

An explicit `model =` overrides a provider default. A `custom` provider accepts
an OpenAI-compatible `base_url`, but its security, privacy, and output contract
are entirely the caller's responsibility.

## What leaves the machine

For a remote provider, AutoXplainR sends a text prompt containing aggregate
metrics, metric definitions, feature names, explanation summaries, and
diagnostic cautions. It excludes raw rows, fitted objects, case-level
predictions, and API keys. The attached `narrative_provenance` attribute records
the requested provider, provider used, resolved model, remote/local status, and
fallback status.

This minimization does not make a remote call private. Feature or target names
can themselves be sensitive, and provider logging and training policies still
apply. Rename sensitive columns or use the local template or Ollama when those
names must not leave the machine.

## Interpretation boundary

Schema-conforming prose can still be wrong, incomplete, or misleading. The
generated memo is subordinate to the numerical report. It may summarize
association and predictive evidence, but it cannot establish causality,
fairness, safety, compliance, or deployment readiness. Always review it against
the computed results before sharing it.

Cloudflare documents JSON schemas for a specific supported-model list, which
does not currently name the GPT-OSS default used here. AutoXplainR therefore
does not advertise structured output for that adapter even though the model's
request schema exposes a `response_format` field.
