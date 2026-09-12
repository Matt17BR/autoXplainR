# Mobile accessibility scan state

Browser CI at `44d6372` reported a target-size violation on the score selector
at 390 pixels. The post-scan screenshot showed a normally sized control, but it
did not record the geometry during the scan.

Three local action replays and eight phase-controlled checks showed that the
old two-frame wait allowed axe to run during smooth scrolling. Every scan after
scrolling stopped passed at the same final position. Returning keyboard focus
to the score selector exposed its complete 204-by-36-pixel target; all nine
sampled hit points worked, and the arrow key changed the displayed metric.
The intermittent CI violation did not recur locally. Moving geometry is a
plausible explanation, not a reproduced causal result.

The test now waits for 150 milliseconds of unchanged scroll and element
geometry, with a four-second failure bound. It preserves the open details and
keyboard focus and checks that geometry stays unchanged throughout axe's scan.
Failures retain the complete axe diagnostics. Product styles and accessibility
rules are unchanged. A separate HTML fixture verifies that axe still rejects
genuinely small adjacent controls after the same settling step.

The complete supplied-model suite passed 181 checks on the existing fixture
and 184 on newly generated CI fixtures. The count differs because the older
fixture withheld one timing measurement; the fresh fixture completed all three
models' repeats. The fresh run includes both desktop and 390-pixel views,
keyboard interactions, print checks, and the negative target-size fixture.
`mobile-axe-review.json` records versions, input and script hashes, geometry
observations, and acceptance results. Full diagnostics and screenshots remain
in the stated cache directory.
