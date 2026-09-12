# AutoXplainR release checklist

GitHub releases and CRAN submissions have separate completion criteria.
For each manual check, record **passed**, **pending**, or **not
applicable**, with evidence and the tested commit/archive. A CI run does
not stand in for a manual review.

## Prepare a GitHub release

- Confirm version/date consistency in `DESCRIPTION`, `NEWS.md`,
  `CITATION.cff` and release records. Label `cran-comments.md` as a
  preparation record until the corresponding archive is actually
  submitted.
- Review contributors and third-party material in `PROVENANCE.md`;
  retain required attribution and redistribution notices.
- Review the public workflow, statistical interpretation, failure states
  and narrative wording. Generated prose must not claim guaranteed
  grounding.
- Regenerate documentation; run tutorials, lint, spelling and URL
  checks. Inspect permanent missing URLs and canonical-form failures.
  Record transient publisher failures separately.
- Run the complete test suite, optional-engine matrix, numerical
  references and coverage. Record skipped integrations and known
  numerical limitations.
- Review test quality for changed behavior. Require the browser and
  imputation mutation checks to reject their deliberate faults for the
  intended reasons; an execution error is not a successful negative
  control.
- Check candidate/fold values, selection thresholds and failed-refit
  explanations. Check distributions, associations and source-record
  links against original supplied tables, including imputation, novel
  categories and row removal.
- Verify every binary cutoff and calibration group against independent
  source predictions. Exercise explicit event-class order and
  existing-model adapters.
- Attempt to reuse scores and explanations after changing model state,
  adapter context, outcomes or row order. Require a clear rejection of
  stale evidence.
- Test summary, sampled-row and no-data exports, including hidden
  payloads. Verify benchmark units, common batches, incomplete runs and
  resolution limits.
- Build once with `.github/scripts/build-check-source.sh`, check that
  exact archive under release R including manuals, and preserve its
  checksum.
- Check the same archive under R-devel. Explain remaining check notes.
- Require the configured Windows, macOS and Linux CI gates and the live
  H2O integration to pass. A hosted narrative live test is optional and
  must be identified as run or skipped.
- Complete both user journeys in `validation/product-walkthrough.md` on
  the installed package and actual regression, binary and multiclass
  reports. Record answers, friction, repairs and repeated tasks in the
  release record. Open generated report examples on desktop and a narrow
  viewport. Review numerical labels, unavailable diagnostics, keyboard
  navigation and print output. Inspect actual PDF pages and font sizes;
  text extraction alone is insufficient.
- Verify that logs, fixtures and shared reports contain no credentials
  or unintended private data.

## Publish on GitHub

- Start from a clean reviewed `main` commit after required checks pass.
- Create an annotated version tag. Do not move an existing published
  tag.
- Inspect the version-tag workflow’s checked archive, release notes and
  checksum. Publish only after the configured release gates succeed.
- Download the published archive, verify its checksum, install it in a
  fresh library and run the documented fit/predict/report workflow.
- Record the final tag, archive digest, CI links and manual-check
  results in the version-specific release record. Win-builder and CRAN
  are not prerequisites for this GitHub completion status.

## Optional: submit that release to CRAN

Complete this section only when a CRAN submission is intended. Record
**not applicable: GitHub release only** otherwise.

- Review current CRAN requirements and the exact proposed source
  archive.
- Submit that archive to Win-builder R-devel and retain its result email
  or URL. Never reuse results for an older archive.
- Update `cran-comments.md` with actual checks, notes and submission
  context.
- The maintainer submits the reviewed archive and records reviewer
  feedback.
- After acceptance, verify CRAN installation, citations and website
  release links. Until then, describe the package as available on
  GitHub, not CRAN-approved.

Corrections to published package code require a new version and tag.
Changes to release evidence must distinguish the published artifact from
later main-branch configuration or documentation changes.
