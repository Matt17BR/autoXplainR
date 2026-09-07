# Optional narrative providers

AutoXplainR can summarize evaluation scores, retained explanation
findings, fitted effects and failed checks in prose. The default is a
deterministic local narrative and does not send a request, even if API
keys are present in the environment.

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

The package validates response fields and length and appends
interpretation limits locally. It does not verify generated numbers or
enforce factual grounding. Review generated claims against the numerical
output; prompt instructions and a valid response schema do not guarantee
correctness. Transport behavior is tested with mocked requests. The
optional live Gemini test is not a statistical validation test and is
not required to use or test the local workflow.

The result supplies all retained components automatically. An explicit
`audit` overrides audit findings and importance while preserving
evaluation and effects. `importance_data` and `pdp_data` override only
their respective components. No explanation computation or model fitting
is repeated to write a memo.

Attached audits and effects must identify the same fitted model and
evaluation data. Identified importance tables receive the same check.
Bare importance tables remain available as explicitly labeled
user-supplied evidence with unverified identity; set their `metric`
attribute rather than implying that the package measured their values.
