# Executed workflow comparison

On 7 September 2026, both AutoXplainR and DALEX + modelStudio generated working
HTML explanations from the same fitted models and held-out rows. Their common
evaluation metrics agreed exactly. This establishes a reproducible workflow
comparison; it does **not** establish that AutoXplainR is easier, faster, more
accurate, or preferable for its intended users. Those claims remain unmeasured.

## Design and versions

[compare-workflows.R](compare-workflows.R) generates 400 public synthetic
delivery records with five predictors: distance in kilometres, package count,
traffic index, dispatch hour, and rain. It fixes 300 training rows and 100 test
rows before fitting, using seed `20260907`. The response is delivery time in
minutes. A pre-specified linear primary model and intercept baseline are fitted
once through `autoxplain(..., model_set = "quick", explain = FALSE)`.

Both explanation branches then receive those **same R model objects**, ordered
predictor values, outcomes, and evaluation rows. The script asserts their
identity and prediction agreement. This is an end-to-end data-to-HTML exercise
with a shared fitting stage and two subsequent explanation/report paths, not a
comparison of independent training or tuning systems. No model is selected using
the test outcomes. The small, clean regression example does not cover missing
values, classification, grouped or temporal sampling, distribution shift, or
large-data performance.

The executed runtime was R 4.5.2, AutoXplainR working source 0.4.0, DALEX 2.5.4,
modelStudio 3.1.2, r2d3 0.2.6 and iml 0.11.4. Reference packages were installed
in `/tmp/autoxplain-workflow-library`, without replacing shared installations.
The [session record](results/workflow-comparison-session.txt) contains the full
runtime, exact split indices and data hash. This run precedes the release commit;
it is not a claim about a subsequently changed source archive.

The established path follows the authors' documented workflow:
[`DALEX::explain()`](https://modeloriented.github.io/DALEX/reference/explain.html)
accepts the fitted model, reference predictors and outcomes;
[`modelStudio()`](https://modelstudio.drwhy.ai/reference/modelStudio.html)
consumes the explainer and creates an interactive widget. We generate one studio
per model and use the authors' documented
[`r2d3::save_d3_html()` export](https://modelstudio.drwhy.ai/#save--share).
The online primary documentation was checked on 7 September 2026, alongside the
installed package help for the versions above. Online example output can show
older package versions; the session record identifies what actually ran.

## Numerical results

All six metric comparisons have absolute error zero:

| Shared fit | RMSE, minutes | MAE, minutes | R-squared |
| --- | ---: | ---: | ---: |
| Primary linear model | 5.180033 | 4.093404 | 0.936966 |
| Intercept baseline | 20.658174 | 15.781203 | -0.002524 |

DALEX's native RMSE and R-squared are compared directly. MAE is explicitly the
mean absolute value of its retained residuals; its `mad` measure is median
absolute error and is not relabelled as MAE. See the exact values and methods in
[workflow-comparison-metrics.csv](results/workflow-comparison-metrics.csv).

Both permutation calculations use all 100 test rows, 20 shuffles per feature,
and increase in RMSE. Their internal random-number consumption differs, so the
[importance means](results/workflow-comparison-importance.csv) are independent
Monte Carlo estimates rather than matched permutation draws. Their differences
are not a quality ranking or an exact numerical equivalence check.

Independent [`iml::FeatureEffect`](https://giuseppec.github.io/iml/reference/FeatureEffect.html)
checks use the same observations and requested grid. PDP values differ by at
most `1.21e-13`; ALE shapes, anchored at the common minimum to remove differing
finite-bin centering conventions, differ by at most `1.43e-14`. See
[workflow-comparison-effects.csv](results/workflow-comparison-effects.csv).
The separately executed [run-reference.R](run-reference.R) adds four tree PDP
checks, four anchored ALE checks, and three analytic ALE value/centering checks
with irregular spacing and ties. All eleven errors are below `5.69e-14` in
[reference-agreement.csv](results/reference-agreement.csv). These are bounded
numerical checks, not proof of general inferential validity.

## Artifacts and task coverage

The actual outputs are under `/tmp/autoxplain-workflow-comparison/`. The
repository retains their [manifest](results/workflow-comparison-artifacts.csv)
and measurements, rather than several large generated HTML files.

| Task in this executed workflow | AutoXplainR | DALEX + modelStudio |
| --- | --- | --- |
| Identify the split and compare primary/baseline metrics | Native report | DALEX measures plus an explicitly labelled script-generated index |
| Inspect permutation importance and fitted feature effects | Native report and retained objects | Native modelStudio panels and retained DALEX objects |
| Share HTML without an R server | One native report | Two native studios with a local comparison index |
| Inspect individual case explanations | Not generated in this workflow | Native break-down and Shapley panels |
| Read AutoXplainR's uncertainty/dependence scope | Native scoped diagnostics | No matching AutoXplainR audit was added to this branch |
| Demonstrate better novice task success | Not measured | Not measured |

This table describes the scripts and artifacts that ran; it is not a complete
package feature inventory. In particular, script glue is not credited as native
modelStudio UI, and absence from this workflow is not a claim that another
package cannot implement a task. modelStudio's documented panels already include
global importance, profiles, and local explanations; an interactive explanation
dashboard alone is not a new capability.
[Official panel documentation](https://modelstudio.drwhy.ai/reference/modelStudio.html).

All four HTML files passed an offline Chromium 145.0.7632.6 smoke check with
HTTP and HTTPS blocked: visible content loaded, native reports contained SVG,
and no JavaScript page errors occurred. The AutoXplainR disclosure opened, each
modelStudio feature selector changed to `traffic_index`, and both index links
were present. The [browser record](results/workflow-comparison-browser.csv)
matches the manifest's SHA-256 hashes. This checks rendering and selected
controls, not every panel, accessibility, interpretation, or human usability.
modelStudio telemetry was explicitly disabled. No live LLM was used.

## Reproduction and unresolved claims

Run from the repository root with its development dependencies installed:

```sh
mkdir -p /tmp/autoxplain-workflow-library
Rscript -e 'install.packages(c("DALEX", "modelStudio", "iml"), lib = "/tmp/autoxplain-workflow-library", repos = "https://cloud.r-project.org")'
AUTOXPLAIN_COMPARISON_LIBRARY=/tmp/autoxplain-workflow-library Rscript validation/compare-workflows.R
Rscript -e '.libPaths(c("/tmp/autoxplain-workflow-library", .libPaths())); source("validation/run-reference.R")'
```

The install command obtains currently available versions; use the recorded
versions when reproducing this particular run. The script never installs
packages itself and stops if prerequisites are absent. To repeat the optional
browser check, set `AUTOXPLAIN_COMPARISON_PYTHON` to a Python interpreter with
Playwright and Chromium installed when running the comparison script. The
executed check used `/tmp/autoxplain-screenshot-env/bin/python`. Regeneration
can change HTML hashes because generated metadata changes.

No timing comparison was recorded because the report generators perform
different work. Code length and artifact size are not usability measurements.
The [user-study protocol](user-study-protocol.md) defines a possible next check;
no participants have been recruited and no human results exist. Practical
advantage for novice analysts, real-data robustness, and broad differentiation
remain unestablished by this comparison.
