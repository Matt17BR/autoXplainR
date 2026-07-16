# Plot aligned cross-model effects

Plot aligned cross-model effects

## Usage

``` r
# S3 method for class 'autoxplain_model_effects'
plot(x, ...)
```

## Arguments

- x:

  An `autoxplain_model_effects` object.

- ...:

  Additional arguments passed to
  [`graphics::matplot()`](https://rdrr.io/r/graphics/matplot.html).

## Value

`x`, invisibly.

## Details

Grid points below `x$min_support` are omitted rather than joined by a
visually authoritative line. Plot styles supplied through `...` replace
defaults and are reused in the legend.
