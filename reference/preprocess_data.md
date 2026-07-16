# Prepare modeling data with an auditable recipe

Performs conservative type normalization and missing-value handling. The
fitted recipe is returned and can be applied to evaluation data with the
same factor levels and imputation values. Identifier removal and ordinal
coercion are opt-in because guessing either can silently change the
scientific question.

## Usage

``` r
preprocess_data(
  data,
  target_column,
  enable_target_handling = TRUE,
  enable_character_to_factors = TRUE,
  enable_ordered_factors = FALSE,
  enable_ordinal_factors = FALSE,
  enable_id_removal = FALSE,
  missing_value_strategy = "keep",
  missing_column_threshold = 0.5,
  novel_level_strategy = c("error", "mode"),
  verbose = FALSE,
  handle_missing = NULL,
  convert_characters = NULL,
  remove_id_columns = NULL
)
```

## Arguments

- data:

  Data frame to prepare.

- target_column:

  Outcome column name.

- enable_target_handling:

  Convert a character outcome to a factor.

- enable_character_to_factors:

  Convert character predictors to factors.

- enable_ordered_factors:

  Convert explicitly ordered predictors to their integer scores.
  Defaults to `FALSE` because spacing may not be meaningful.

- enable_ordinal_factors:

  Convert factors whose levels are integer-like strings to numeric.
  Defaults to `FALSE`.

- enable_id_removal:

  Remove columns with identifier-like names. Defaults to `FALSE`; unique
  values alone never trigger removal.

- missing_value_strategy:

  One of `"keep"`, `"drop_rows"`, `"drop_columns"`, or `"impute"`.
  Legacy names `"remove_rows"` and `"remove_columns"` are accepted.

- missing_column_threshold:

  Fraction missing above which `drop_columns` removes a predictor.

- novel_level_strategy:

  How evaluation-time predictor categories absent from training are
  handled. `"error"` stops; `"mode"` maps them to the most frequent
  observed training category and records the mapping count.

- verbose:

  Emit concise progress messages.

- handle_missing, convert_characters, remove_id_columns:

  Deprecated logical aliases retained for compatibility.

## Value

A list containing `data`, a structured `preprocessing_log`, data
summaries, and a reusable `recipe`.
