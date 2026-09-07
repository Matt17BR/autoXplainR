# Optional narrative providers

AutoXplainR can turn aggregate analysis evidence into prose. The default
is a deterministic local narrative and does not send a request, even if
API keys are present in the environment.

``` r

result <- autoxplain(iris, "Species")
memo <- generate_natural_language_report(result)
render_model_report(result, "report.html", narrative = memo)
narrative_providers()
```

Hosted or local model generation requires an explicit provider:

``` r

# Configure the provider credentials described by narrative_providers() first.
memo <- generate_natural_language_report(result, provider = "gemini")
# Other adapters: groq, cloudflare, openrouter, ollama.
```

Use
[`?generate_natural_language_report`](https://matt17br.github.io/autoXplainR/reference/generate_natural_language_report.md)
for model overrides, environment variables, endpoints, timeout and
fallback behavior. Provider defaults are recorded in the returned
provenance. Availability, pricing and model names can change; consult
the provider’s own documentation when configuring an account.

The package constructs prompts from aggregate evidence. It excludes raw
training rows, per-case predictions and fitted model objects. Feature
names, aggregate statistics and diagnostic text can still be sensitive.
Review them before choosing a hosted provider. Do not commit API keys or
provider response captures containing private data.

Structured output constrains response format, not truth. The package
validates fields and appends interpretation limits locally; generated
claims still need review against the numerical output. Transport
behavior is tested with mocked requests. The optional live Gemini test
is not a statistical validation test and is not required to use or test
the local workflow.
