# Proposed user-study protocol — not executed

This is a study plan, not validation evidence. No participants have been
recruited, no sessions have occurred, and there are no human task-success or
timing results. The executed computational comparison is documented in
[workflow-comparison.md](workflow-comparison.md).

The question is whether the report workflow helps intended novice analysts
reach correct, appropriately limited conclusions compared with an established
DALEX + modelStudio workflow using the same fitted models and evaluation data.
The study must allow either workflow to perform better, or neither to do so.

## Preparation and recruitment

Freeze the package source, dependency versions, scripts, generated reports and
answer keys before recruiting. Prepare two comparable synthetic regression
datasets with realistic names, an explicit train/test split, a primary model,
an intercept baseline, known dependence, and a feature with weak shuffle
evidence. Each workflow receives identical fits and evaluation rows within each
dataset. Confirm numerical agreement and working artifacts before use.

Recruit 12–16 adults for a formative pilot who can read a data frame and run an
R script but have not used either explanation workflow regularly. Exclude
package contributors and people who helped write the tasks. Record basic R and
statistical familiarity, prior exposure to each tool, and accessibility needs.
This convenience pilot cannot establish a population-wide advantage. A later
confirmatory study requires a separately justified sample size and a registered
analysis plan based on a meaningful improvement in decision accuracy.

Obtain informed consent and permission for any screen/audio recording. Use
participant codes and synthetic data; collect no credentials or confidential
datasets. Define recording retention and withdrawal rules before sessions.
No recruitment messages or data collection are authorized by this document.

## Assignment and session

Use a balanced crossover design, randomizing both workflow order and dataset
assignment so participants see each dataset only once. Both workflows get the
same written introduction, time for basic orientation, and task instructions.
Use the same machine, browser size and network conditions. Explain the native
outputs and the reference workflow's labelled metric index without coaching
answers. Log assistance requests and technical failures.

Participants first open a prepared project, run its frozen data-to-report script,
and locate the resulting report. Record command/run failures separately from
interpretation tasks so a dependency problem does not silently exclude someone.
Then ask the following shared tasks using the report, retained evidence and
provided package documentation:

1. Identify which rows were used for training and evaluation, and whether the
   displayed error is a training or test estimate.
2. Report the primary model's RMSE with target units, compare it with the
   baseline, and explain what the observed difference supports.
3. Identify the feature with the largest observed permutation loss increase and
   distinguish model reliance from a causal effect or intrinsic feature value.
4. Read a specified fitted feature-effect change and explain whether the plot
   alone shows what an intervention would do.
5. Explain what variation across repeated shuffles measures and what uncertainty
   it leaves out, including changes to training data and model selection.
6. Explain how the specified dependent features affect interpretation and what
   a small pairwise association screen can and cannot establish.

Use tasks both workflows can address from their supplied evidence. Do not score
modelStudio's local explanations or AutoXplainR-specific diagnostic field names
as shared-task criteria. Missing or hard-to-find evidence may be recorded as a
workflow limitation, but the rubric must not assume that one UI's terminology
is the correct answer. Permit "this output does not establish that" when true.

## Scoring and analysis

Before sessions, publish an answer key with exact row counts, metric values,
feature identities, numerical tolerances, and acceptable scope explanations.
Each task receives 0 for an incorrect substantive conclusion, 1 for a partly
correct or incomplete answer, and 2 for a correct answer with its material
limitation. Score unsupported causal claims explicitly as errors. Set the same
time limit per task in advance; an unfinished task remains in the results.

Two reviewers independently score de-identified responses with workflow labels
removed where possible. Record disagreements and their adjudication; acknowledge
that wording or screenshots may reveal the workflow. Report all participant
scores and paired score differences, including failed runs and unfinished tasks.
The primary pilot outcome is correctness across the six tasks. Secondary
outcomes are task completion, assistance requests, time among completed tasks
with correctness shown alongside it, and a common satisfaction questionnaire.
Use timings only with identical start/stop definitions and disclose order effects.

For this small pilot, show descriptive paired results and uncertainty without
claiming a powered superiority test. Report dataset and order effects, prior
experience, technical failures, and contrary findings. Keep optional qualitative
comments separate from task scores. Do not retrospectively remove difficult
tasks, select only successful participants, or turn favourable quotes into
evidence of general ease of use. A confirmatory claim needs its own prospective
design, recruitment, analysis and actual results.
