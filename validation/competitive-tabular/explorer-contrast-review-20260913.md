# Follow-up review of explorer contrast checks

Reviewed on 13 September 2026. No confirmed contrast or clipping failure was
found among the **355 original incomplete node occurrences** in the exact
regression report. One optional readability refinement remains below. This
resolves those occurrences through additional evidence; it does not turn the
original 1,382 automated passes into a blanket accessibility-conformance claim.

The original record contains 28 scan entries across seven tabs and widths
320, 390, 768 and 1440 pixels. Twenty-four entries contain incomplete findings;
the 355 occurrences refer to 236 distinct CSS targets. The reviewed report is
`integration/explorer-cases-gallery-final/regression.html`, with SHA-256
`15b098aa3a6e82ddad843611c15d6c68ff9e5f0a5ae3b091a4ca4bfb710405b8`.

## Results by group

| Axe reason | Occurrences | Additional evidence | Assessment |
| --- | ---: | --- | --- |
| `imgNode` | 202 | SVG text fill against actual white or paper backgrounds | No confirmed contrast failure |
| `shortTextContent` | 95 | Numeric SVG labels are actual text, with the same computed-color checks | No confirmed contrast failure |
| `elmPartiallyObscured` | 45 | Inactive mobile navigation links extend outside their horizontal scroll region; contrast is 10.247:1 | Intentional scrolling; all tabs remain reachable |
| `nonBmp` | 12 | Graphical legend symbols with adjacent visible text labels | All exceed the 3:1 graphical threshold |
| `elmPartiallyObscuring` | 1 | Sidebar footer at 1440px; two unobscured text rectangles, contrast 7.885:1 | No reproduced text overlap |

All 355 targets have exactly one rendered match and semantically identical
HTML to the source findings. The only serialization differences are attribute
order in navigation links. Nondecorative text contrast ranges from **6.683:1
to 11.576:1**. Computed opacity is one, and the relevant ancestor backgrounds
contain no background images. SVG checks use `fill`, rather than incorrectly
treating CSS `color` as the painted text color.

The 297 SVG occurrences are not clipped by their SVG or other scroll-region
ancestors. Ninety-nine initially below-fold labels were separately scrolled
into view and checked again: all had an unobscured text-center hit in the
viewport. Every initially visible text-center check also passed.

At both 320px and 390px, keyboard ArrowRight navigation traversed all seven
tabs. All fourteen focus/selection states matched the active page and placed
the complete selected tab inside the navigation viewport. Screenshots confirm
that inactive tab text is intentionally cropped at the horizontal edges;
the selected tab is fully readable.

The 1440px sidebar-footer warning did not reproduce as clipping or overlap.
Its text occupies y=944–979 within the 1000px viewport; both text centers hit
the footer itself, and its containing sidebar supplies the dark background.

## Optional P3 refinement

The hollow fold legend marker, `.selection-key-fold`, uses `#78908b` against
`#f6f7f3`, giving **3.167980678:1**. It looks faint at its small size. A darker
or thicker marker would improve readability; no production change was made.

This is a graphical symbol accompanied by the visible word “fold”, not a
normal text label. Its adjacent text has 5.970:1 contrast. The other symbols
have 6.532:1 and 4.866:1 contrast. Classification as graphics follows their
visual role and adjacent labels, rather than assuming that `aria-hidden`
alone removes a contrast requirement. W3C specifies 4.5:1 for ordinary text
and 3:1 for meaningful graphical objects; it also cautions that thin shapes
can look faint despite nominally passing. See
[text contrast](https://www.w3.org/WAI/WCAG22/Understanding/contrast-minimum.html)
and [non-text contrast](https://www.w3.org/WAI/WCAG22/Understanding/non-text-contrast.html).

## Evidence and limits

The [machine-readable review](explorer-contrast-review-20260913.json) retains
all 355 measurements, source/report/script hashes, 99 scrolled-label checks,
14 keyboard checks, inspected screenshot hashes, and inspection-attempt
history. Full computed-style ancestry is retained at the recorded cache path.
The [read-only Playwright probe](review-explorer-contrast.py) uses the same
Chromium 145.0.7632.6 / Playwright 1.58.0 runtime and aborts network requests.

Representative screenshots inspected were overview and data at 320px,
selection at 390px, and feature effects and predictions at 1440px. No
clipped chart labels or selected navigation labels were observed in them.

The initial scroll extension needed two harness corrections: choosing the
unique rendered target when hidden charts reused a selector, and completing
scroll actions before checking visibility. Earlier attempts are retained and
are not counted as completed visibility evidence. The final v5 run passes
all 99 scrolled-label checks.

This review uses computed colors, ancestor clipping geometry, text-center hit
testing, and representative visual inspection. It is not pixel-exhaustive
occlusion analysis, assistive-technology testing, a participant study, or
coverage of every model/feature state. The original automated record remains
unchanged, including its incomplete findings. No native modeling jobs,
acceptance-data reads, or production edits were performed for this review.
