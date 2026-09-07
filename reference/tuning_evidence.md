# Inspect the evidence behind a tuning decision

Returns aggregate search, candidate, fold and selection records for
reports. No models are fitted or rescored. Source row identities and
per-case predictions are omitted; feature names and diagnostic messages
may remain.

## Usage

``` r
tuning_evidence(result)
```

## Arguments

- result:

  An
  [`autoxplain()`](https://matt17br.github.io/autoXplainR/reference/autoxplain.md)
  result.

## Value

A versioned list of tuning evidence, or an explicit not-run record.
