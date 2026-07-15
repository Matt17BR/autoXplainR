# Generate an evidence-constrained narrative

Produces a deterministic local narrative by default. Remote or locally
hosted generative models are used only when `provider` is set
explicitly. Raw rows, fitted objects, case-level predictions, and
secrets are never included in the prompt. Generated prose remains
secondary to the computed evaluation and explanation evidence.

## Usage

``` r
generate_natural_language_report(
  autoxplain_result,
  importance_data = NULL,
  pdp_data = NULL,
  model_characteristics = NULL,
  audit = NULL,
  provider = c("local", "gemini", "groq", "cloudflare", "ollama", "openrouter", "custom"),
  api_key = NULL,
  model = NULL,
  base_url = NULL,
  account_id = NULL,
  max_tokens = 1000L,
  temperature = 0.2,
  timeout = 30,
  fallback = TRUE,
  use_remote = NULL,
  transport = NULL
)
```

## Arguments

- autoxplain_result:

  An `autoxplain_result` or `autoxplain_audit`.

- importance_data:

  Optional permutation importance data.

- pdp_data:

  Optional feature-effect list.

- model_characteristics:

  Optional model metadata.

- audit:

  Optional `autoxplain_audit`.

- provider:

  One of `"local"`, `"gemini"`, `"groq"`, `"cloudflare"`, `"ollama"`,
  `"openrouter"`, or `"custom"`. The default is always `"local"`.

- api_key:

  Provider API key. For an explicitly selected hosted provider, the
  corresponding environment variable shown by
  [`narrative_providers()`](https://matt17br.github.io/autoXplainR/reference/narrative_providers.md)
  is consulted when this is `NULL`.

- model:

  Model identifier. `NULL` uses the provider default shown by
  [`narrative_providers()`](https://matt17br.github.io/autoXplainR/reference/narrative_providers.md).
  A custom provider requires an explicit model.

- base_url:

  Optional endpoint override. A custom provider requires it.

- account_id:

  Cloudflare account ID. When `NULL`, `CLOUDFLARE_ACCOUNT_ID` is
  consulted for the Cloudflare provider.

- max_tokens:

  Maximum response tokens.

- temperature:

  Sampling temperature.

- timeout:

  Request timeout in seconds.

- fallback:

  Return the deterministic narrative when the remote call fails.

- use_remote:

  Deprecated compatibility switch. `FALSE` forces local generation;
  `TRUE` with no explicit provider selects Gemini. Prefer `provider`.

- transport:

  Optional advanced request function for testing or custom networking.
  It receives one request list and must return narrative text.

## Value

A single character string with a `narrative_provenance` attribute.
