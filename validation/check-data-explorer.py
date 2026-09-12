"""Check report data tasks against original supplied rows, offline in Chromium.

Generate inputs with render-explorer-cases.R and render-exploration-fixtures.R.
This is implementer acceptance automation, not a participant usability study.
Mutation checks deliberately corrupt data and a displayed axis to establish
that wrong missing counts, source rows and scales fail the relevant task.
"""

import argparse
import hashlib
import re
from playwright.sync_api import sync_playwright
from pathlib import Path
import json, math, bisect
from report_payload import decode_data_payload

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--case-dir", type=Path, required=True)
parser.add_argument("--output-dir", type=Path, required=True)
parser.add_argument("--axe-path", type=Path, required=True)
parser.add_argument(
    "--cases",
    nargs="+",
    choices=["binary", "messy-regression"],
    default=["binary", "messy-regression"],
)
args = parser.parse_args()
folder = args.case_dir.resolve()
out = args.output_dir.resolve()
out.mkdir(parents=True, exist_ok=True)
checks = []


def check(name, passed, evidence=None):
    checks.append(dict(name=name, passed=bool(passed), evidence=evidence))


def ranks(values):
    order = sorted(range(len(values)), key=lambda i: values[i])
    result = [0.0] * len(values)
    i = 0
    while i < len(order):
        end = i + 1
        while end < len(order) and values[order[end]] == values[order[i]]:
            end += 1
        for j in range(i, end):
            result[order[j]] = (i + 1 + end) / 2
        i = end
    return result


def spearman(pairs):
    a = ranks([x for x, y in pairs])
    b = ranks([y for x, y in pairs])
    mean = (len(a) + 1) / 2
    return sum((x - mean) * (y - mean) for x, y in zip(a, b)) / math.sqrt(
        sum((x - mean) ** 2 for x in a) * sum((y - mean) ** 2 for y in b)
    )


def raw_missing_matches(page, oracle):
    text = page.locator("#data-summary").inner_text()
    return all(
        str(oracle[split + "_weight_missing"]) + " missing" in text
        for split in ["training", "evaluation"]
    )


def source_row_matches(page, oracle):
    original = oracle["raw"]["evaluation"][0]
    text = page.locator("#data-selected-row").inner_text()
    if "test_data row 1" not in text:
        return False
    for name in ["distance_km", "delivery_hours", "dispatch_backlog"]:
        cells = (
            page.locator("#data-selected-row tr")
            .filter(has_text=name)
            .locator("td")
            .all_text_contents()
        )
        if len(cells) != 3 or not math.isclose(
            float(cells[1].replace(",", "")),
            original[name],
            rel_tol=0.001,
            abs_tol=0.001,
        ):
            return False
    return True


def histogram_matches(page, oracle, prefix, scale="percent"):
    train = [
        r["weight_kg"] for r in oracle["raw"]["training"] if r["weight_kg"] is not None
    ]
    low = min(train)
    high = max(train)
    breaks = [low + (high - low) * i / 24 for i in range(25)]
    series = []
    for split in ["training", "evaluation"]:
        counts = [0] * 26
        rows = oracle["raw"][split]
        for row in rows:
            value = row["weight_kg"]
            if value is None:
                continue
            bin = bisect.bisect_right(breaks, value)
            if value == breaks[-1]:
                bin = len(breaks) - 1
            counts[bin] += 1
        series.extend(
            counts
            if scale == "count"
            else (100 * count / len(rows) for count in counts)
        )
    geometry = page.locator("#data-distribution svg").evaluate(
        """svg=>({bars:[...svg.querySelectorAll('rect')].map(n=>({height:+n.getAttribute('height'),y:+n.getAttribute('y')})),
        yt:[...svg.querySelectorAll('[data-axis-tick=y]')].map(n=>({value:Number(n.textContent.replaceAll(',','')),label:n.textContent,y:+n.getAttribute('y')-5})),
        xt:[...svg.querySelectorAll('[data-axis-tick=x]')].map(n=>({value:Number(n.textContent.replaceAll(',','')),x:+n.getAttribute('x')})),
        left:+svg.querySelector('line.data-grid').getAttribute('x1'),right:+svg.querySelector('line.data-grid').getAttribute('x2')})"""
    )
    ticks = geometry["yt"]
    calibrated = len(ticks) >= 2 and ticks[0]["value"] == 0
    pixels_per_unit = (
        (ticks[0]["y"] - ticks[-1]["y"]) / ticks[-1]["value"]
        if calibrated and ticks[-1]["value"] > 0
        else float("nan")
    )
    check(
        prefix
        + f": {scale} histogram geometry matches original counts and visible axis",
        calibrated
        and len(geometry["bars"]) == len(series)
        and all(
            abs(bar["height"] - value * pixels_per_unit) < 1e-8
            and abs(bar["y"] + bar["height"] - ticks[0]["y"]) < 1e-8
            for bar, value in zip(geometry["bars"], series)
        ),
    )
    check(
        prefix + f": {scale} ticks have readable precision and a consistent scale",
        calibrated
        and all(
            (
                float(tick["value"]).is_integer()
                if scale == "count"
                else (
                    len(tick["label"].split(".")[-1]) <= 1
                    if "." in tick["label"]
                    else True
                )
            )
            for tick in ticks
        )
        and all(
            abs(tick["y"] - (ticks[0]["y"] - tick["value"] * pixels_per_unit)) < 1e-8
            for tick in ticks
        ),
    )
    bin_width = (geometry["right"] - geometry["left"]) / 26
    check(
        prefix + f": {scale} numeric ticks retain the original input coordinates",
        bool(geometry["xt"])
        and all(
            low <= tick["value"] <= high
            and abs(
                tick["x"]
                - (
                    geometry["left"]
                    + bin_width
                    + (tick["value"] - low) / (high - low) * 24 * bin_width
                )
            )
            < 1e-8
            for tick in geometry["xt"]
        ),
    )


def chart_layout(page):
    page.evaluate(
        "async()=>{await new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)));}"
    )
    return page.evaluate(
        """() => {
      const visible = node => node.getBoundingClientRect().width > 0 && node.checkVisibility({checkVisibilityCSS:true});
      const svgs = [...document.querySelectorAll('#data svg')].filter(visible).filter(n=>!n.closest('.data-plot-scroll'));
      const overflow = svgs.filter(n=>{let r=n.getBoundingClientRect();return r.left < -1 || r.right > innerWidth+1}).map(n=>n.getAttribute('aria-label'));
      const clipped=[],small=[];
      const elementOverflow=[...document.querySelectorAll('#data *')].filter(visible).filter(n=>!n.closest('.table-wrap,.data-plot-scroll')).filter(n=>{let r=n.getBoundingClientRect();return r.left < -1 || r.right > innerWidth+1}).map(n=>n.tagName+':'+(n.id||String(n.className)));
      svgs.forEach(svg=>{
        const frame=svg.getBoundingClientRect(),scale=frame.width/svg.viewBox.baseVal.width;
        svg.querySelectorAll('text').forEach(t=>{
          const r=t.getBoundingClientRect();
          if(r.left<frame.left-1||r.right>frame.right+1||r.top<frame.top-1||r.bottom>frame.bottom+1)clipped.push(t.firstChild.textContent);
          if(parseFloat(getComputedStyle(t).fontSize)*scale<13.9)small.push(t.firstChild.textContent);
        });
      });
      return {overflow,clipped,small,elementOverflow,pageOverflow:document.documentElement.scrollWidth>innerWidth};
    }"""
    )


def mobile_column_tasks(browser, path, prefix):
    page = browser.new_page(viewport={"width": 390, "height": 844}, reduced_motion="reduce")
    page.goto(path.as_uri() + "#data")
    selector = page.locator("#data-column-select")
    initial = selector.input_value()
    values = selector.locator("option").evaluate_all("nodes=>nodes.map(n=>n.value)")
    alternate = next(value for value in values if value != initial)
    check(prefix + ": compact mobile column control is available",
          selector.is_visible() and not page.locator(".data-column-list").is_visible())
    selector.select_option(alternate)
    check(prefix + ": mobile column choice updates heading and distribution",
          page.locator("#data-variable-title").inner_text() == alternate
          and page.locator("#data-distribution svg").get_attribute("aria-label") == "Distribution of " + alternate)
    selector.select_option(initial)
    check(prefix + ": mobile column choice can return to the outcome",
          page.locator("#data-variable-title").inner_text() == initial)
    for width in (390, 320):
        page.set_viewport_size({"width": width, "height": 844})
        help_button = page.locator('[aria-controls="data-distribution-note"]')
        help_button.focus()
        check(prefix + f": histogram help is readable by keyboard at {width}px",
              page.locator("#data-distribution-note").is_visible()
              and "Percentages use all rows in each split" in page.locator("#data-distribution-note").inner_text()
              and not chart_layout(page)["elementOverflow"])
        page.keyboard.press("Escape")
        check(prefix + f": histogram help can be dismissed at {width}px",
              not page.locator("#data-distribution-note").is_visible())
        selector.focus()
    page.set_viewport_size({"width": 1440, "height": 1000})
    page.locator('[data-column-name=' + json.dumps(alternate) + ']').click()
    check(prefix + ": desktop selection synchronizes the mobile control",
          selector.input_value() == alternate)
    page.set_viewport_size({"width": 390, "height": 844})
    check(prefix + ": selected column survives a viewport change",
          selector.is_visible() and selector.input_value() == alternate
          and page.locator("#data-variable-title").inner_text() == alternate)
    help_button.focus()
    page.locator("#data-distribution-note").evaluate("node=>{node.style.left='-400px'}")
    check(prefix + ": negative control rejects a visibly clipped tooltip",
          "SPAN:data-distribution-note" in chart_layout(page)["elementOverflow"])
    page.close()


def mutation_checks(browser):
    oracle = json.loads((folder / "messy-regression-oracle.json").read_text())
    pattern = re.compile(
        r'(<script[^>]*id="axr-data-payload"[^>]*>)(.*?)(</script>)', re.S
    )
    for label, mode in [("missing-count", "summary"), ("source-mapping", "rows")]:
        html = (folder / ("messy-regression-" + mode + ".html")).read_text()
        match = pattern.search(html)
        if match is None:
            raise ValueError("Missing fixture payload")
        data = decode_data_payload(json.loads(match.group(2)))
        if label == "missing-count":
            data["profile"]["stages"]["raw"]["columns"]["weight_kg"]["training"][
                "n_missing"
            ] += 7
        else:
            a = next(r for r in data["rows"] if r["row_key"] == "test_data:1")
            b = next(r for r in data["rows"] if r["row_key"] == "test_data:21")
            a["row_key"], b["row_key"] = b["row_key"], a["row_key"]
        encoded = (
            json.dumps(data, ensure_ascii=True)
            .replace("<", r"\u003c")
            .replace(">", r"\u003e")
            .replace("&", r"\u0026")
        )
        altered = html[: match.start(2)] + encoded + html[match.end(2) :]
        path = out / ("mutated-" + label + ".html")
        path.write_text(altered)
        page = browser.new_page(viewport={"width": 1440, "height": 1000})
        page.goto(path.as_uri() + "#data")
        if label == "missing-count":
            page.locator("[data-column-name=weight_kg]").click()
            detected = not raw_missing_matches(page, oracle)
        else:
            page.evaluate('AutoXplainRData.selectRow("test_data:1")')
            detected = not source_row_matches(page, oracle)
        check("negative control: rejects " + label, detected)
        page.close()
    page = browser.new_page(viewport={"width": 1440, "height": 1000})
    page.goto((folder / "messy-regression-summary.html").as_uri() + "#data")
    page.locator("[data-column-name=weight_kg]").click()
    page.locator("#data-distribution [data-axis-tick=y]").evaluate_all(
        "nodes=>nodes.forEach(node=>{node.textContent=Number(node.textContent.replaceAll(',',''))*2})"
    )
    start = len(checks)
    histogram_matches(page, oracle, "wrong-axis")
    probes = checks[start:]
    del checks[start:]
    check(
        "negative control: rejects displayed scale without changing bars",
        any(
            "histogram geometry" in item["name"] and not item["passed"]
            for item in probes
        ),
    )
    page.close()


with sync_playwright() as p:
    b = p.chromium.launch()
    for kind in args.cases:
        for mode in ["summary", "rows", "none"]:
            page = b.new_page(viewport={"width": 1440, "height": 1000})
            errors = []
            page.on("pageerror", lambda e: errors.append(str(e)))
            page.route("http://**/*", lambda r: r.abort())
            page.route("https://**/*", lambda r: r.abort())
            page.goto((folder / (kind + "-" + mode + ".html")).as_uri() + "#data")
            prefix = kind + "/" + mode
            check(
                prefix + ": artifact recorded",
                True,
                hashlib.sha256(
                    (folder / (kind + "-" + mode + ".html")).read_bytes()
                ).hexdigest(),
            )
            if mode == "none":
                check(
                    prefix + ": no data workspace or data payload",
                    page.locator("#data").count() == 0
                    and page.locator("#axr-data-payload").count() == 0,
                )
                check(
                    prefix + ": no individual case links",
                    page.locator("[data-select-row]").count() == 0,
                )
                page.close()
                continue
            payload = decode_data_payload(page.evaluate(
                'JSON.parse(document.getElementById("axr-data-payload").textContent)'
            ))
            if mode == "summary":
                check(
                    prefix + ": summary contains no row payload",
                    payload["rows"] is None
                    and page.locator("[data-select-row]").count() == 0,
                )
            check(
                prefix + ": one initial workspace",
                page.locator("[data-data-panel]:visible").count() == 1,
            )
            check(
                prefix + ": starts with outcome",
                page.locator("#data-variable-title").inner_text()
                == payload["profile"]["target"],
            )
            page.locator("[data-data-view=relationships]").focus()
            page.keyboard.press("Enter")
            check(
                prefix + ": keyboard opens relationships and hides irrelevant bars",
                page.locator("#relationships").is_visible()
                and not page.locator("#data-scale-control").is_visible(),
            )
            page.locator("[data-data-view=records]").focus()
            page.keyboard.press("Space")
            check(
                prefix + ": keyboard opens records",
                page.locator("#data-records-panel").is_visible(),
            )
            page.locator("[data-data-view=distribution]").click()
            if kind == "binary":
                box = page.locator("#data-distribution svg").bounding_box()
                check(
                    prefix + ": binary distribution compact",
                    box["height"] < 160,
                    box["height"],
                )
                page.evaluate("window.scrollTo(0, 0)")
                page.screenshot(
                    path=str(out / (prefix.replace("/", "-") + "-distribution.png")),
                    full_page=True,
                )
                page.locator("[data-data-view=relationships]").click()
                check(
                    prefix + ": binary rate visible",
                    page.locator("#data-conditional").is_visible()
                    and "Observed event rate"
                    in page.locator("#data-conditional").inner_text(),
                )
                target = payload["profile"]["target"]
                positive = payload["profile"]["positive"]
                x = page.locator("#data-y").input_value()
                oracle = json.loads((folder / "binary-data-oracle.json").read_text())
                check(
                    prefix + ": event identity matches source contract",
                    positive == oracle["positive"],
                )
                profile = payload["profile"]
                names = [c["name"] for c in profile["columns"]]
                pair = profile["stages"]["raw"]["pairs"][
                    "_".join(
                        str(i)
                        for i in sorted([names.index(target) + 1, names.index(x) + 1])
                    )
                ]
                axis = profile["stages"]["raw"]["columns"][x]["axis"]
                br = axis["breaks"]
                expected_rates = []
                for split in ["training", "evaluation"]:
                    grouped = {}
                    for row in oracle["raw"][split]:
                        v = row[x]
                        y = row[target]
                        if v is None or y is None:
                            continue
                        code = bisect.bisect_right(br, v) + 1
                        if v == br[-1]:
                            code = len(br)
                        group = grouped.setdefault(code, [0, 0])
                        group[0] += 1
                        group[1] += int(y == positive)
                    got = pair[split]["conditional_event"]
                    got = [got] if isinstance(got, dict) else got
                    okay = all(
                        r["n"] == grouped[r["x"]][0]
                        and r["events"] == grouped[r["x"]][1]
                        and abs(r["rate"] - grouped[r["x"]][1] / grouped[r["x"]][0])
                        < 1e-12
                        for r in got
                    )
                    check(prefix + ": independently counted " + split + " events", okay)
                    expected_rates.extend(
                        grouped[r["x"]][1] / grouped[r["x"]][0] for r in got
                    )
                plot = page.locator("#data-conditional svg")
                ticks = plot.locator("text").evaluate_all(
                    '(nodes)=>nodes.map(n=>({label:n.firstChild.textContent,y:Number(n.getAttribute("y"))}))'
                )
                zero = next(t["y"] - 5 for t in ticks if t["label"] == "0%")
                one = next(t["y"] - 5 for t in ticks if t["label"] == "100%")
                geometry = plot.locator("circle").evaluate_all(
                    '(nodes)=>nodes.map(n=>Number(n.getAttribute("cy")))'
                )
                check(
                    prefix + ": event point geometry matches original outcomes",
                    len(geometry) == len(expected_rates)
                    and all(
                        abs((zero - y) / (zero - one) - rate) < 1e-10
                        for y, rate in zip(geometry, expected_rates)
                    ),
                )
                page.locator("#data-pair > details > summary").click()
                texts = page.locator("#data-pair svg text").evaluate_all(
                    "(nodes)=>nodes.map(node=>node.firstChild.textContent)"
                )
                check(
                    prefix + ": both real binary labels visible",
                    all(label in texts for label in ["no", "yes"])
                    and not any(
                        "Other train" in t or "New in eval" in t for t in texts
                    ),
                    texts,
                )
                page.locator("#data-pair > details > summary").click()
                page.evaluate("window.scrollTo(0, 0)")
                page.screenshot(
                    path=str(out / (prefix.replace("/", "-") + "-relationships.png")),
                    full_page=True,
                )
            else:
                oracle = json.loads(
                    (folder / "messy-regression-oracle.json").read_text()
                )
                page.locator("[data-column-name=weight_kg]").click()
                check(
                    prefix + ": raw missing counts visible",
                    raw_missing_matches(page, oracle),
                )
                weight_button = page.locator("[data-column-name=weight_kg]")
                total_missing = (
                    oracle["training_weight_missing"]
                    + oracle["evaluation_weight_missing"]
                )
                check(
                    prefix + ": column badge uses current raw populations",
                    f"{total_missing} missing" in weight_button.inner_text(),
                )
                page.select_option("#data-split", "evaluation")
                check(
                    prefix + ": column badge follows selected evaluation split",
                    f'{oracle["evaluation_weight_missing"]} missing'
                    in weight_button.inner_text()
                    and "evaluation" in weight_button.get_attribute("aria-description"),
                )
                page.select_option("#data-split", "both")
                histogram_matches(page, oracle, prefix)
                page.select_option("#data-scale", "count")
                histogram_matches(page, oracle, prefix, scale="count")
                page.select_option("#data-scale", "percent")
                page.screenshot(
                    path=str(
                        out / (prefix.replace("/", "-") + "-numeric-distribution.png")
                    ),
                    full_page=True,
                )
                page.locator("[data-data-view=relationships]").click()
                for split in ["training", "evaluation"]:
                    pairs = [
                        (r["weight_kg"], r["delivery_hours"])
                        for r in oracle["raw"][split]
                        if r["weight_kg"] is not None
                        and r["delivery_hours"] is not None
                    ]
                    expected = spearman(pairs)
                    text = (
                        page.locator("#data-association p")
                        .filter(has_text=split + ":")
                        .inner_text()
                    )
                    match = re.search(r"\(signed\) ([-+0-9.e]+).*n = ([0-9,]+)", text)
                    check(
                        prefix + ": unbinned " + split + " signed association",
                        match is not None
                        and math.isclose(float(match.group(1)), expected, abs_tol=5e-5)
                        and int(match.group(2).replace(",", "")) == len(pairs),
                        text,
                    )
                page.locator("[data-data-view=distribution]").click()
                page.locator("#data-stage").select_option("processed")
                check(
                    prefix + ": imputed counts visible",
                    page.locator("#data-summary").inner_text().count("0 missing") == 2,
                )
                check(
                    prefix + ": processed column badge does not retain raw missingness",
                    "missing" not in weight_button.inner_text(),
                )
                page.locator("#data-stage").select_option("raw")
                page.locator("[data-column-name=service]").click()
                page.locator(".data-values > summary").click()
                novel = (
                    page.locator("#data-distribution-table tr")
                    .filter(has_text="New in evaluation")
                    .inner_text()
                )
                check(
                    prefix + ": novel raw categories counted",
                    novel.split()[-2:]
                    == ["0", str(len(oracle["evaluation_new_service_rows"]))],
                    novel,
                )
                page.locator("#data-stage").select_option("processed")
                novel = (
                    page.locator("#data-distribution-table tr")
                    .filter(has_text="New in evaluation")
                    .inner_text()
                )
                check(
                    prefix + ": mapped categories counted",
                    novel.split()[-2:] == ["0", "0"],
                    novel,
                )
                page.locator(".data-values > summary").click()
                page.locator("#data-stage").select_option("raw")
                check(
                    prefix + ": removed IDs not embedded",
                    "parcel-001" not in page.content()
                    and "parcel-241" not in page.content(),
                )
                if mode == "rows":
                    page.locator("[data-data-view=records]").click()
                    page.locator("#data-filter-column").select_option("weight_kg")
                    page.locator("#data-filter-op").select_option("missing")
                    page.locator("#data-filter-form button[type=submit]").click()
                    check(
                        prefix + ": missing filter independent count",
                        page.evaluate("AutoXplainRData.getState().matchingRows")
                        == oracle["training_weight_missing"]
                        + oracle["evaluation_weight_missing"],
                    )
                    check(
                        prefix + ": column badge describes filtered sample",
                        f"{total_missing} missing" in weight_button.inner_text()
                        and "Filtered exported sample"
                        in weight_button.get_attribute("aria-description"),
                    )
                    page.evaluate(
                        """()=>{
                        window.selectedSourceEvents=[];
                        window.addEventListener('axr:row-selected',event=>selectedSourceEvents.push(event.detail));
                        AutoXplainRData.selectView('distribution');AutoXplainRData.selectRow('test_data:1');
                        }"""
                    )
                    check(
                        prefix
                        + ": source selection announces one completed row identity",
                        page.evaluate("selectedSourceEvents")
                        == [
                            {
                                "row_key": "test_data:1",
                                "partition": "evaluation",
                                "processed_position": 1,
                            }
                        ],
                    )
                    state = page.evaluate("AutoXplainRData.getState()")
                    check(
                        prefix + ": source selection opens records and clears filters",
                        state["view"] == "records"
                        and state["selected"] == "test_data:1"
                        and state["split"] == "evaluation"
                        and state["filters"] == [],
                    )
                    detail = (
                        page.locator("#data-selected-row tr")
                        .filter(has_text="weight_kg")
                        .inner_text()
                    )
                    expected = oracle["training_weight_median"]
                    check(
                        prefix + ": selected original missing and fitted imputation",
                        "Missing" in detail
                        and math.isclose(
                            float(detail.split()[-1]), expected, rel_tol=0.001
                        ),
                        detail,
                    )
                    check(
                        prefix + ": selected source values match original row",
                        source_row_matches(page, oracle),
                    )
                    check(
                        prefix + ": ten row page",
                        page.locator("#data-row-table tbody tr").count() == 10,
                    )
                    page.locator("[data-column-name=weight_kg]").click()
                    page.locator("#data-y").select_option("delivery_hours")
                    page.locator("#data-scatter-details > summary").click()
                    points = page.locator("#data-row-scatter circle")
                    first = points.first.get_attribute("data-row-key")
                    points.first.focus()
                    page.keyboard.press("ArrowRight")
                    page.keyboard.press("Enter")
                    selected = page.evaluate("AutoXplainRData.getState().selected")
                    check(
                        prefix + ": arrow and enter inspect another source point",
                        selected == points.nth(1).get_attribute("data-row-key")
                        and selected != first,
                    )
                    page.locator("#data-scatter-details > summary").click()
                    page.evaluate("window.scrollTo(0, 0)")
                    page.screenshot(
                        path=str(out / "messy-rows-records.png"), full_page=True
                    )
            for view in ["distribution", "relationships", "records"]:
                page.locator("[data-data-view=" + view + "]").click()
                if view == "relationships" and kind == "binary":
                    page.locator("#data-pair > details").evaluate(
                        "(node)=>{node.open=true}"
                    )
                if view == "records" and mode == "rows" and kind == "messy-regression":
                    page.locator("#data-scatter-details").evaluate(
                        "(node)=>{node.open=true}"
                    )
                check(
                    prefix + ": one " + view + " workspace",
                    page.locator("[data-data-panel]:visible").count() == 1,
                )
                for width in [1440, 768, 390, 320]:
                    page.set_viewport_size({"width": width, "height": 900})
                    layout = chart_layout(page)
                    check(
                        prefix + ": " + view + " readable chart layout " + str(width),
                        not layout["overflow"]
                        and not layout["clipped"]
                        and not layout["small"]
                        and not layout["elementOverflow"]
                        and not layout["pageOverflow"],
                        layout,
                    )
                    check(
                        prefix + ": " + view + " no page overflow " + str(width),
                        page.evaluate(
                            "document.documentElement.scrollWidth<=innerWidth"
                        ),
                    )
                page.set_viewport_size({"width": 1440, "height": 1000})
            page.add_script_tag(path=str(args.axe_path.resolve()))
            for view in ["distribution", "relationships", "records"]:
                page.locator("[data-data-view=" + view + "]").click()
                result = page.evaluate(
                    'async()=>{let r=await axe.run(document.getElementById("data"),{runOnly:{type:"tag",values:["wcag2a","wcag2aa","wcag21a","wcag21aa"]}});return {violations:r.violations.map(x=>({id:x.id,n:x.nodes.length})),incomplete:r.incomplete.map(x=>x.id)}}'
                )
                check(
                    prefix + ": " + view + " automatic component accessibility",
                    not result["violations"],
                    result,
                )
            check(prefix + ": browser errors", not errors, errors)
            page.close()
            mobile_column_tasks(b, folder / (kind + "-" + mode + ".html"), prefix)
    if "messy-regression" in args.cases:
        mutation_checks(b)
    version = b.version
    b.close()
(out / "data-explorer-checks.json").write_text(
    json.dumps(dict(browser=version, checks=checks), indent=2)
)
print(
    json.dumps(
        dict(
            passed=sum(r["passed"] for r in checks),
            checks=len(checks),
            failures=[r for r in checks if not r["passed"]],
        ),
        indent=2,
    )
)
assert all(r["passed"] for r in checks)
