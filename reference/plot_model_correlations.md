# Plot prediction agreement among retained AutoML models

Plot prediction agreement among retained AutoML models

## Usage

``` r
plot_model_correlations(autoxplain_result, test_data = NULL)
```

## Arguments

- autoxplain_result:

  An `autoxplain_result`.

- test_data:

  Optional evaluation data.

## Value

A Plotly heatmap.

## Details

Classification compares predicted class labels (the fraction in
agreement), using a 0.5 probability cutoff for binary outcomes.
Regression compares signed Spearman correlations; constant predictions
have no defined correlation. Neither measure establishes prediction
accuracy.
