# Reproduce a local narrative

This example generates a narrative from the evidence produced by the installed
package. It requires no API key or network request:

```r
library(AutoXplainR)
result <- autoxplain(iris, "Species", seed = 2026)
memo <- generate_natural_language_report(result)
cat(memo)
render_model_report(result, "iris-report.html", narrative = memo)
```

Check the narrative against `result$evaluation` and the retained explanation
audit. The result contains the model, split, metric definitions and numerical
evidence; the narrative is an additional presentation of that evidence.

See [provider setup](LLM_PROVIDERS.md) for optional generated wording. Historical
provider responses are not presented here as evidence that an endpoint or model
is currently available.
