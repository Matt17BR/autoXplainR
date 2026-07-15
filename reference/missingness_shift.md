# Compare raw missing-value rates between training and evaluation data

`missingness_shift()` compares each predictor's missing-value rate
before imputation or other missing-value handling. A changed missingness
pattern can be a useful clue that data collection, population, or
pipeline behavior differs between model fitting and evaluation.

## Usage

``` r
missingness_shift(result, threshold = 0.05)
```

## Arguments

- result:

  An `autoxplain_result` with retained preprocessing metadata for both
  training and evaluation data.

- threshold:

  Absolute missing-rate difference used to flag a predictor. The default
  is 0.05, or five percentage points.

## Value

An `autoxplain_missingness_shift` object containing a predictor-level
`features` data frame.

## Details

The threshold is a practical flag, not a hypothesis test. This
diagnostic only concerns missing values; it does not establish that the
full predictor distribution is stable or shifted.

## Examples

``` r
training <- data.frame(x = c(NA, 2:40), y = 1:40)
evaluation <- data.frame(x = c(rep(NA, 5), 46:60), y = 41:60)
fit <- autoxplain(training, "y", test_data = evaluation)
missingness_shift(fit)
#> <AutoXplainR missingness shift>
#>   data:        40 training + 20 evaluation rows
#>   predictors:  1 with any missing values
#>   flagged:     1 model inputs at 5 percentage points
#>   caution:     practical flag; not a statistical test or general drift check
```
