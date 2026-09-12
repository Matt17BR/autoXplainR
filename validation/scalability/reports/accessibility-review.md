# Accessibility items requiring review

The browser job for commit `5163fb1` reported no automated violations, but it
also returned checks that axe could not decide. Those checks were reviewed
before release. Zero automated violations alone was not treated as a complete
accessibility verdict.

## What was inspected

The CI artifact contained 28 exploration scan records: 24 had incomplete
contrast checks, eight also had an incomplete ARIA check, and four were empty.
The four prediction scans had 30, 14, 14 and 27 incomplete contrast nodes.
The old acceptance scripts saved rule names and counts without the targets.

A bounded local reproduction recovered full diagnostics for six regression
views at 390 pixels and the prediction view of all four fixtures at 1440 pixels.
The prediction node counts matched CI exactly. Local report hashes differ
from CI because report generation records local timing and provenance, so this
is a review of the same components rather than a byte-identical CI replay.
Screenshots, computed colors, ancestor backgrounds, actual accessibility trees
and keyboard navigation were inspected. The review used Playwright 1.58.0,
Chromium 145.0.7632.6 and axe 4.13.0.

## Two names needed proper group semantics

The comparison summary and feature-importance controls had `aria-label` on
ordinary `div` elements. Their contents remained available, but their intended
group names were not reliably exposed. Chromium's accessible-role lookup
found neither named group in the original report.

Both containers now have `role="group"`. The regenerated report exposes one
named comparison group with its three terms and values, and one named
importance group per inspected model with its feature controls. The browser
suite checks the actual accessible names and descendants. No chart, score,
model or style was changed. This follows the distinction between a nameless
[generic container](https://www.w3.org/TR/wai-aria-1.2/#generic) and a named
[group](https://www.w3.org/TR/wai-aria-1.2/#group).

## Contrast disposition

All 177 contrast-node occurrences in the representative scans fell into these
four cases. They did not require color changes.

| axe could not decide because | Occurrences | Review |
| --- | ---: | --- |
| SVG image content | 109 | The actual SVG text fill, opacity and background were inspected. |
| Very short text, such as an axis tick | 46 | These are ordinary chart labels; the same text contrast check applies. |
| Partially obscured element | 19 | These are inactive tabs outside the horizontally scrolling mobile navigation. |
| A symbol rather than ordinary text | 3 | These are the filled-circle, open-circle and dashed-line legend keys, each with a separate readable text label. |

The minimum SVG text contrast was **6.68:1** against the actual white or
`#f6f7f3` background, above the 4.5:1 requirement for ordinary text. Inactive
navigation text was **10.25:1** against the dark sidebar. All seven mobile tabs
were reachable with the right-arrow key, and each selected tab scrolled fully
into view. The cropped inactive tabs were therefore scrollable content, not
an obstructed active control.

The weakest legend graphic was the open-circle outline at **3.17:1** against
the paper background. It is a graphical key, not a letter: it is `aria-hidden`,
its adjacent word "fold" has **5.97:1** contrast, and the chart also exposes
fold values as text. Its outline exceeds the graphical-object 3:1 requirement.
See the W3C guidance for [text contrast](https://www.w3.org/WAI/WCAG22/Understanding/contrast-minimum.html)
and [non-text contrast](https://www.w3.org/WAI/WCAG22/Understanding/non-text-contrast.html).

The attached JSON records source hashes, rule counts, exact color pairs and
the before/after accessible-tree control. Full raw diagnostics and screenshots
are retained in the release cache. Future exploration and prediction runs now retain the
complete incomplete-check details as well as their compact rule list, so a
future reviewer can inspect the target and reason directly. No axe rule was
suppressed. This focused review does not substitute for a screen-reader study
or claim that every accessibility requirement has been exhaustively tested.

## Acceptance after the repair

The complete explorer replay passed **1,382 checks**, including 17 checks of
the actual named groups and descendants. Its 28 regression scans had zero
automated violations and no remaining ARIA incomplete checks. The independent
chart checker passed 219 checks after 24 literal R-oracle checks. The complete
prediction suite passed **1,923 checks**, including the deliberate wrong-model
mutation, three export modes and no-JavaScript behavior. Both browser scripts
retained full contrast diagnostics, with no page errors.

The cache also retains two early setup failures from missing separate fixture
files in the new output directory. Those files were generated before the
complete successful replays; no product assertion was changed to pass them.
