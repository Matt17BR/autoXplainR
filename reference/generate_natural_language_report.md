# Summarize retained model evidence in prose

Produces a deterministic local narrative by default. Remote or locally
hosted generative models are used only when `provider` is set
explicitly. Evaluation, retained explanation findings, fitted effects
and failed checks are included automatically. Prompt construction omits
raw rows, fitted objects and case-level predictions; names and
diagnostic text may still be sensitive. Hosted output is checked for
format and length, not numerical grounding. Review generated claims
against the computed evidence.

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
  max_tokens = 4000L,
  temperature = NULL,
  timeout = 30,
  structured = TRUE,
  fallback = TRUE,
  use_remote = NULL,
  transport = NULL
)
```

## Arguments

- autoxplain_result:

  An `autoxplain_result` or `autoxplain_audit`.

- importance_data:

  Optional permutation importance table overriding the retained audit's
  importance component. Tables with an explainer identity must match the
  primary model. Bare tables are labeled user-supplied with unverified
  identity; their metric should be supplied as a `metric` attribute.

- pdp_data:

  Optional feature-effect list overriding retained effects. An empty
  list omits effects from the narrative.

- model_characteristics:

  Retained for compatibility. Generic model-family capacity descriptions
  are not included in the memo.

- audit:

  Optional `autoxplain_audit` overriding retained audit findings and
  importance. The result's evaluation and effects remain available.

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

  Maximum provider generation-token budget. Gemini reasoning and
  response text share this budget, so completion is not guaranteed. The
  default Gemini model requests low thinking. Generated content is
  limited to 500 words; fixed interpretation notes are appended
  afterward.

- temperature:

  Sampling temperature. `NULL` uses 1 for Gemini, following its current
  model guidance, and 0.2 for other providers.

- timeout:

  Request timeout in seconds.

- structured:

  Request a validated five-section JSON response when the selected
  provider supports it. AutoXplainR renders the validated fields and
  adds fixed interpretation boundaries locally. See
  [`narrative_providers()`](https://matt17br.github.io/autoXplainR/reference/narrative_providers.md)
  for current capability declarations.

- fallback:

  Return the deterministic narrative when the remote call fails.

- use_remote:

  Deprecated compatibility switch. `FALSE` forces local generation;
  `TRUE` with no explicit provider selects Gemini. Prefer `provider`.

- transport:

  Optional advanced request function for testing or custom networking.
  It receives one request list and must return response text; when
  `request$structured` is `TRUE`, that text must be schema-conforming
  JSON.

## Value

A single character string with a `narrative_provenance` attribute.
