# Repeated native scoring uses the same budget

Clarified on 12 September 2026 after the first full-training native fit began,
before any full-data evaluation scoring was attempted.

If a saved native fit needs another scoring attempt, all recorded earlier
scoring processes for that exact fit count toward the original combined wall
limit, including failed attempts. A new scoring cohort does not reset that
limit. The runner also refuses to start while a recorded scoring process for
the same fit remains active. The fit itself and its declared algorithm are
unchanged by this accounting clarification.
