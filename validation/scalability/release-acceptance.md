# Release-archive acceptance for 0.7.0

These checks extend the existing release gates. They are a test plan until the
final archive has passed them; source-checkout results do not substitute for an
installed-archive run.

| Capability | Required acceptance |
| --- | --- |
| Native categorical boosting | A small three-class workflow fits five rounds with explicit native encoding. CV folds and the retained blueprint must record native encoding. A fresh R process must restore predictions and declared class order after factor-level reordering. The rendered HTML's probability matrix and all 60 evaluation source identities must match the original inputs and saved predictions. |
| Automatic BAM | One smooth on 10,000 training rows triggers the real automatic solver policy. Both 5,000-row CV folds and the final fit must use BAM; the recorded decision must retain its original automatic request and outer-training planning size. |
| Explanation sampling | The BAM workflow scores all 2,000 evaluation rows, while explanations use 37. Independent RMSE must match both stored model scores and audit performance before and after reload. |
| Pair sampling | Report pairs use 53 rows per partition, with full population sizes recorded. Univariate counts still cover all 10,000 training and 2,000 evaluation rows. |
| Compressed report storage | The installed archive renders all 12,000 explicitly exported rows. A separate process decodes the actual HTML blocks, follows raw/processed aliases and compares every exported value and original row identity with the supplied frames. Fixture values are dyadic, so the numeric comparison is exact. At least one real compressed block must be exercised. |
| Bundled browser code | The report must contain the codec, payload loader and explorer code read from the installed package, with its codec license present. The separate browser gate verifies that code in Chromium, Firefox and WebKit, including malformed-block and offline behavior. |

`check-installed-artifact.R` continues to run the existing regression, binary,
multiclass, saved-recipe and supplied-model cases. It starts
`check-reloaded-artifact.R` with `Rscript --vanilla`, which must restore models
without attaching their engine packages. The new checks do not skip missing
engines in the release environment: the release workflow already provisions
XGBoost and mgcv.

The native-engine filters now include `boosting-native`, `additive-solvers`,
`forest-call`, `mars-call` and `regularized-call`
on Linux and Windows and in the isolated exact-minimum-engine job. The complete
release suite also runs these tests. The core R-version/platform matrix remains
separate; optional XGBoost acceptance is not claimed for every core matrix entry.
Neural iteration and call-reconstruction tests run in the unfiltered core suite
on every configured R platform, because `nnet` is a required dependency.

The source-package job builds once, checks that archive, verifies its checksum,
installs it into a fresh library, runs these smoke checks, and verifies the
checksum again. The scripts accept the expected version and enforce the exact
library path. Final artifacts and logs remain outside the source package.

Before building, regenerate manuals with the pinned roxygen2 version and commit
`DESCRIPTION`, `NAMESPACE` and `man/` changes. The relevant new help includes
`explanation_rows`, `max_pair_rows`, additive solver choices and boosting input
encoding. Existing CI fails if regeneration changes those files. In particular,
removing `explanation_rows` does not remove PDP's separate curve-row limit.

The compact check establishes packaging, decoding and data fidelity. It is not
an additional browser usability test or a timing benchmark. The small native
fits establish archive/reload compatibility, not model quality on hard data.

The two new installed-package cases passed locally on Linux with a preserved
0.7.0 source snapshot. A fresh `--vanilla` process restored both model types,
verified full evaluation scores and sampled explanation counts, and decoded nine
compressed blocks. All 12,000 exported rows matched the supplied data exactly in
both raw and processed form. Expected pair identity and two complete CV folds
are asserted before their counts or settings are checked, so missing records
cannot make those checks pass vacuously. This preliminary run selected only the
two new cases; the complete gate must still run against the final checked archive.

Local evidence is retained under the task cache in `acceptance-smoke-source`,
`acceptance-smoke-source-manifest.json`, `acceptance-smoke-runner`,
`acceptance-smoke-new.log` and `acceptance-smoke-output/fresh-session.log`.
The runner records the exact gate-file hashes and its deliberately reduced scope.
The extended native probability/source checks passed in the same preserved
preliminary library; their separate logs and runner have the `-v2` suffix.
