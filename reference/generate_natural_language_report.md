# Generate an evidence-constrained narrative

Produces a deterministic local narrative by default, or sends only an
aggregated analysis context to the Gemini API when an API key is
supplied. Raw rows, model objects, and predictions are never included in
the prompt. The generated text is explicitly secondary to the numeric
audit.

## Usage

``` r
generate_natural_language_report(
  autoxplain_result,
  importance_data = NULL,
  pdp_data = NULL,
  model_characteristics = NULL,
  audit = NULL,
  api_key = NULL,
  model = "gemini-3.5-flash",
  max_tokens = 1000L,
  temperature = 0.2,
  use_remote = TRUE,
  fallback = TRUE
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

- api_key:

  Gemini API key. When `NULL`, `GEMINI_API_KEY` is consulted. If no key
  is available, a local deterministic narrative is returned.

- model:

  Gemini model identifier.

- max_tokens:

  Maximum response tokens.

- temperature:

  Sampling temperature.

- use_remote:

  Whether remote generation is allowed. Set `FALSE` to guarantee
  local-only operation.

- fallback:

  Return the deterministic narrative when the remote call fails.

## Value

A single character string.
