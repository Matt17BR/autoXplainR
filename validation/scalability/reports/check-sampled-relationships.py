"""Check bounded browser pair analysis against source data and an R full-row oracle."""
import argparse
import json
import math
from pathlib import Path
import re
from playwright.sync_api import sync_playwright
from browser_runtime import launch

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--folder", type=Path, required=True)
parser.add_argument("--browsers", nargs="+", default=["chromium"])
args = parser.parse_args()
folder = args.folder.resolve()
source = json.loads((folder / "source.json").read_text())
checks = []


def check(name, passed, evidence=None):
    checks.append(dict(name=name, passed=bool(passed), evidence=evidence))


def reservoir(n, limit, seed, split):
    """Independent integer implementation of the documented browser stream."""
    mask = 0xffffffff
    state = (seed ^ (0x9e3779b9 if split == "training" else 0x85ebca6b)) & mask
    selected = list(range(min(n, limit)))
    for i in range(limit, n):
        state = (state + 0x6d2b79f5) & mask
        value = ((state ^ (state >> 15)) * (state | 1)) & mask
        value = (value ^ ((value + (((value ^ (value >> 7)) * (value | 61)) & mask)) & mask)) & mask
        position = int(((value ^ (value >> 14)) & mask) / 4294967296 * (i + 1))
        if position < limit:
            selected[position] = i
    assert len(set(selected)) == min(n, limit)
    assert all(0 <= i < n for i in selected)
    return sorted(selected)


def correlation(x, z):
    assert len(set(x)) == len(x) and len(set(z)) == len(z), "This fixture has no rank ties"
    ranks = lambda values: {index: rank for rank, index in enumerate(sorted(range(len(values)), key=values.__getitem__))}
    a, b = ranks(x), ranks(z)
    n = len(x)
    return 1 - 6 * sum((a[i] - b[i]) ** 2 for i in range(n)) / (n * (n * n - 1))


def choose_column(page, name):
    if page.locator("#data-column-select").is_visible():
        page.select_option("#data-column-select", name)
    else:
        page.locator(f'[data-column-name="{name}"]').click()


expected = {}
for split, offset, n in [("training", 0, source["training_rows"]),
                          ("evaluation", source["training_rows"], source["evaluation_rows"])]:
    indices = reservoir(n, source["pair_limit"], source["pair_seed"], split)
    expected[split] = dict(n=len(indices), rho=correlation(
        [source["x"][offset + i] for i in indices], [source["z"][offset + i] for i in indices]))
    if split == "evaluation":
        check("Rare final record is absent from this fixed sample, making disclosure consequential", n - 1 not in indices)

with sync_playwright() as runtime:
    for engine in args.browsers:
        browser = launch(runtime, engine)
        for file in ("report.html", "all-pairs.html"):
            page = browser.new_page()
            errors = []
            page.on("pageerror", lambda error: errors.append(str(error)))
            page.goto((folder / file).as_uri())
            page.locator('[data-page-link="data"]').click()
            page.locator('[data-data-view="records"]').click()
            page.select_option("#data-filter-column", "x")
            page.select_option("#data-filter-op", "present")
            page.locator('#data-filter-form button[type="submit"]').click()
            check(f"{engine}/{file}: filter still includes every original record",
                  page.evaluate("AutoXplainRData.getState().matchingRows") == source["rows"])
            page.locator('[data-data-view="relationships"]').click()
            choose_column(page, "x")
            page.select_option("#data-y", "z")
            text = page.locator("#data-association").inner_text()
            for split in ("training", "evaluation"):
                match = re.search(split + r": Spearman correlation \(signed\) ([-+.\deE]+) · n = ([\d,]+)", text)
                wanted = expected[split] if file == "report.html" else dict(
                    n=source[split + "_rows"], rho=source["full_spearman"][split])
                check(f"{engine}/{file}: {split} association agrees with the independent source oracle",
                      match is not None and int(match[2].replace(",", "")) == wanted["n"] and
                      math.isclose(float(match[1]), wanted["rho"], rel_tol=5e-4, abs_tol=5e-5),
                      dict(expected=wanted, displayed=match.groups() if match else text))
            if file == "report.html":
                check(engine + ": browser discloses sample and matching population", "300 of 2,500 matching exported rows" in text)
                page.locator('[data-data-view="distribution"]').click()
                choose_column(page, "rare_group")
                # The rare value occurs only in evaluation, so its honest
                # training-based bin is "New in evaluation", not a training
                # category invented from the evaluation data.
                rare = page.locator("#data-distribution-table tbody tr").filter(
                    has=page.get_by_text("New in evaluation", exact=True))
                counts = rare.locator("td").all_text_contents()
                check(engine + ": exact filtered distribution still shows the rare record omitted by pair sampling",
                      counts == ["New in evaluation", "0", "1"], counts)
                page.locator('[data-data-view="relationships"]').click()
                page.select_option("#data-y", "x")
                labels = page.locator("#data-pair-table tbody tr td:nth-child(2)").all_text_contents()
                check(engine + ": sampled joint counts do not invent an unobserved rare group",
                      "New in evaluation" not in labels and "common" in labels)
                choose_column(page, "x")
                page.select_option("#data-y", "z")
                check(engine + ": revisiting a pair reuses the same source-independent sample", page.locator("#data-association").inner_text() == text)
            else:
                check(engine + ": explicit unlimited pair mode has no sampling claim", "Sampled relationships" not in text)
            check(f"{engine}/{file}: no runtime error", not errors, errors)
            page.close()
        browser.close()

result = dict(passed=all(item["passed"] for item in checks), checks=checks,
              oracle="Full correlations computed directly in R; bounded correlations computed from separate original source values using Python integer reservoir sampling and rank differences.")
(folder / "sampled-pair-checks.json").write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps(dict(passed=result["passed"], checks=len(checks), failures=[item for item in checks if not item["passed"]]), indent=2))
raise SystemExit(0 if result["passed"] else 1)
