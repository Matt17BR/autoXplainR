"""Check report Predictions against separate full-precision model outputs.

Generate inputs with validation/render-explorer-cases.R. The JSON prediction_source
records contain original outcomes and public predict() results, never report bins,
cutoff grids, or chart coordinates. This checks implementer acceptance tasks, not
participant usability. Originals are never changed by the wrong-model mutation.
"""

import argparse
import copy
import hashlib
from importlib.metadata import version
import json
import math
from pathlib import Path
import platform
import re

from playwright.sync_api import sync_playwright
from report_payload import decode_data_payload, decode_prediction_payload, read_json_payload, replace_json_payload


def close(actual, expected, tolerance=1e-10):
    return actual is not None and math.isclose(actual, expected, rel_tol=tolerance, abs_tol=tolerance)


def settled(page):
    page.evaluate("()=>new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)))")


def panel_for(page, model_id):
    return page.locator(f"[data-prediction-model={json.dumps(model_id)}]")


def select_model(page, model_id):
    page.locator("#prediction-model-select").select_option(model_id)
    settled(page)
    return panel_for(page, model_id)


def payload(page, identifier):
    value = json.loads(page.locator(f"#{identifier}").text_content())
    if identifier == "axr-data-payload":
        return decode_data_payload(value)
    if identifier == "axr-predictions-payload":
        return decode_prediction_payload(value)
    return value


def counts_at(source, cutoff):
    truth = [value == source["positive"] for value in source["observed"]]
    predicted = [value >= cutoff for value in source["prediction"]]
    return {
        "tp": sum(t and p for t, p in zip(truth, predicted)),
        "fn": sum(t and not p for t, p in zip(truth, predicted)),
        "fp": sum(not t and p for t, p in zip(truth, predicted)),
        "tn": sum(not t and not p for t, p in zip(truth, predicted)),
    }


def displayed_rate(text, numerator, denominator):
    if denominator == 0:
        return text == "Not defined"
    return text.endswith("%") and abs(float(text[:-1]) / 100 - numerator / denominator) <= .000501


def cutoff_checks(page, oracle, check, prefix, indices=range(101)):
    for model_id, source in oracle["prediction_source"].items():
        panel = select_model(page, model_id)
        check(f"{prefix}:{model_id}: selected identity", panel.is_visible())
        slider = panel.locator("[data-prediction-cutoff]")
        check(f"{prefix}:{model_id}: cutoff enabled", slider.is_enabled())
        states = slider.evaluate("""(slider, indices) => indices.map(i => {
            slider.value = String(i); slider.dispatchEvent(new Event('input', {bubbles:true}));
            const panel = slider.closest('[data-prediction-model]');
            return {index:i, threshold:panel.querySelector('[data-cutoff-value]').textContent,
                cells:[...panel.querySelectorAll('[data-confusion-table] td')].map(cell => ({
                    observed:cell.dataset.observed, predicted:cell.dataset.predicted,
                    count:Number(cell.querySelector('[data-cell-count]').textContent),
                    rate:cell.querySelector('[data-cell-rate]').textContent})),
                metrics:Object.fromEntries([...panel.querySelectorAll('[data-cutoff-metric]')].map(
                    field => [field.dataset.cutoffMetric,field.textContent]))};
        })""", list(indices))
        for state in states:
            index = state["index"]
            expected = counts_at(source, index / 100)
            actual = {}
            rates = []
            for cell in state["cells"]:
                obs, pred = cell["observed"] == source["positive"], cell["predicted"] == source["positive"]
                key = ("tp" if pred else "fn") if obs else ("fp" if pred else "tn")
                actual[key] = cell["count"]
                total = expected["tp"] + expected["fn"] if obs else expected["tn"] + expected["fp"]
                rates.append(displayed_rate(cell["rate"], expected[key], total))
            label = f"{prefix}:{model_id}: cutoff {index / 100:.2f}"
            # Mutation acceptance specifically requires this numeric assertion.
            check(label + " counts match independent predictions", actual == expected,
                  None if actual == expected else {"actual": actual, "expected": expected})
            metric = state["metrics"]
            numeric = all(rates) and metric["fp"] == str(expected["fp"]) and metric["fn"] == str(expected["fn"])
            for name, num, den in [
                ("accuracy", expected["tp"] + expected["tn"], len(source["observed"])),
                ("sensitivity", expected["tp"], expected["tp"] + expected["fn"]),
                ("specificity", expected["tn"], expected["tn"] + expected["fp"]),
                ("precision", expected["tp"], expected["tp"] + expected["fp"]),
            ]:
                numeric = numeric and displayed_rate(metric[name], num, den)
            check(label + " rates and metrics update together", numeric)
            check(label + " displayed threshold", close(float(state["threshold"]), index / 100))


def predicted_classes(source):
    labels = source["class_levels"]
    if isinstance(source["prediction"][0], list):
        # Python max returns the first index in a tie: the declared class order.
        winners = [max(range(len(labels)), key=lambda i: row[i]) for row in source["prediction"]]
        return [labels[i] for i in winners], [row[i] for row, i in zip(source["prediction"], winners)]
    negative = next(label for label in labels if label != source["positive"])
    return [source["positive"] if p >= .5 else negative for p in source["prediction"]], source["prediction"]


def confusion_checks(panel, source, check, prefix):
    predicted, _ = predicted_classes(source)
    cells = panel.locator("[data-confusion-table] td").evaluate_all("""nodes => nodes.map(cell => ({
        observed:cell.dataset.observed, predicted:cell.dataset.predicted,
        count:Number(cell.querySelector('[data-cell-count]').textContent),
        rate:cell.querySelector('[data-cell-rate]').textContent}))""")
    check(prefix + ": all declared confusion cells", len(cells) == len(source["class_levels"]) ** 2)
    for cell in cells:
        n = sum(y == cell["observed"] and p == cell["predicted"] for y, p in zip(source["observed"], predicted))
        total = source["observed"].count(cell["observed"])
        check(prefix + f": confusion {cell['observed']}/{cell['predicted']}",
              cell["count"] == n and displayed_rate(cell["rate"], n, total))


def calibration_checks(panel, model, source, check, prefix, leaderboard_gap):
    predictions, confidence = predicted_classes(source)
    multiclass = isinstance(source["prediction"][0], list)
    event = [y == p for y, p in zip(source["observed"], predictions)] if multiclass else [
        y == source["positive"] for y in source["observed"]]
    bins = model["calibration"]["bins"]
    # Reconstruct the documented leaderboard contract independently in Python:
    # average ranks keep ties together; small samples cap the number of groups.
    count = len(confidence)
    effective_bins = min(5, max(1, count // 10))
    ordered = sorted(range(count), key=lambda i: confidence[i])
    groups = {}
    start = 0
    while start < count:
        end = start + 1
        while end < count and confidence[ordered[end]] == confidence[ordered[start]]:
            end += 1
        rank = (start + 1 + end) / 2
        group = min(effective_bins, math.ceil(rank / count * effective_bins))
        groups.setdefault(group, []).extend(ordered[start:end])
        start = end
    check(prefix + ": canonical rank group count", len(bins) == len(groups))
    expected_points, covered = [], []
    for record, members in zip(bins, [groups[key] for key in sorted(groups)]):
        low, high = record["low"], record["high"]
        check(prefix + ": group observed probability range", close(low, min(confidence[i] for i in members))
              and close(high, max(confidence[i] for i in members)))
        covered.extend(members)
        if not members:
            check(prefix + ": calibration bin has source rows", False, record)
            continue
        probability = sum(confidence[i] for i in members) / len(members)
        observed = sum(event[i] for i in members) / len(members)
        expected_points.append((probability, observed, len(members)))
        check(prefix + f": bin {low:.1f} mean, rate, support",
              record["n"] == len(members) and record["correct_or_events"] == sum(event[i] for i in members)
              and close(record["mean_probability"], probability) and close(record["observed_rate"], observed))
    check(prefix + ": calibration partitions every evaluation row", sorted(covered) == list(range(len(confidence))))
    weighted_gap = sum(n * abs(probability - observed) for probability, observed, n in expected_points) / count
    check(prefix + ": plotted groups reconstruct leaderboard calibration gap",
          close(weighted_gap, leaderboard_gap) and close(model["calibration"].get("calibration_error"), weighted_gap))
    figure = panel.locator(".classification-calibration .axr-chart")
    guidance = figure.locator("details.axr-chart-guidance")
    was_open = guidance.evaluate("element => element.open")
    if not was_open:
        guidance.locator("summary").press("Enter")
    check(prefix + ": grouping guidance opens by keyboard", guidance.evaluate("element => element.open"))
    check(prefix + ": grouping scope is stated", "same rank grouping as the leaderboard" in guidance.inner_text())
    if not was_open:
        guidance.locator("summary").press("Enter")
    geometry = figure.evaluate("""figure => {
        const svg = figure.querySelector('svg'), line = svg.querySelector('line.axr-zero');
        const number = (node, key) => Number(node.getAttribute(key));
        return {limits:['xMin','xMax','yMin','yMax'].map(key=>Number(figure.dataset[key])),
            reference:figure.dataset.reference,
            line:line && ['x1','x2','y1','y2'].map(key=>number(line,key)),
            points:[...svg.querySelectorAll('[data-chart-point]')].map(point=>{
                const circle=point.querySelector('.axr-point'); return {
                    x:number(circle,'cx'), y:number(circle,'cy'), r:number(circle,'r'),
                    detail:point.getAttribute('aria-label')};}),
            retained:[...figure.querySelectorAll('[data-chart-source]')].map(point =>
                [Number(point.dataset.x), Number(point.dataset.y), Number(point.dataset.count)]),
            tableRows:figure.querySelectorAll('tbody tr').length};
    }""")
    check(prefix + ": calibration full probability axes and diagonal",
          geometry["limits"] == [0, 1, 0, 1] and geometry["reference"] == "identity" and geometry["line"] is not None)
    check(prefix + ": calibration exact table alternative", geometry["tableRows"] == len(expected_points))
    check(prefix + ": calibration retained aggregates only", len(geometry["retained"]) == len(expected_points) and all(
        all(close(a, b) for a, b in zip(actual, expected)) for actual, expected in zip(geometry["retained"], expected_points)))
    plotted = geometry["line"] is not None and len(geometry["points"]) == len(expected_points)
    if plotted:
        x0, x1, y0, y1 = geometry["line"]
        maximum = max(n for x, y, n in expected_points)
        for point, (x, y, n) in zip(geometry["points"], expected_points):
            plotted = plotted and close(point["x"], x0 + x * (x1 - x0)) and close(point["y"], y0 + y * (y1 - y0))
            plotted = plotted and close(point["r"] ** 2 / 100, n / maximum) and f"rows {n}" in point["detail"]
    check(prefix + ": calibration positions and areas encode mean, rate, support", plotted)


def in_bin(value, low, high, maximum):
    return low <= value < high or value == high == maximum


def regression_checks(model, source, check, prefix):
    observed, predicted = source["observed"], source["prediction"]
    residual = [y - p for y, p in zip(observed, predicted)]
    regression = model["regression"]
    expected = {"rmse": math.sqrt(sum(r * r for r in residual) / len(residual)),
                "mae": sum(abs(r) for r in residual) / len(residual), "bias": sum(residual) / len(residual)}
    # jsonlite serializes the retained R numeric vector in RMSE/MAE/bias order.
    metrics = regression["metrics"]
    if isinstance(metrics, list):
        metrics = dict(zip(["rmse", "mae", "bias"], metrics))
    check(prefix + ": regression metrics from original predictions",
          all(close(metrics[key], value) for key, value in expected.items()))
    cells = regression["density"]
    maximum = max(max(cell["predicted_high"], cell["observed_high"]) for cell in cells)
    assigned = []
    for cell in cells:
        members = [i for i, (y, p) in enumerate(zip(observed, predicted)) if
                   in_bin(p, cell["predicted_low"], cell["predicted_high"], maximum) and
                   in_bin(y, cell["observed_low"], cell["observed_high"], maximum)]
        assigned.extend(members)
        check(prefix + ": occupied observed/predicted bin count", len(members) == cell["n"])
    check(prefix + ": density accounts for each original row once", sorted(assigned) == list(range(len(observed))))
    histogram = regression["residual_histogram"]
    maximum = max(row["high"] for row in histogram)
    check(prefix + ": residual histogram counts", all(row["n"] == sum(
        in_bin(r, row["low"], row["high"], maximum) for r in residual) for row in histogram)
        and sum(row["n"] for row in histogram) == len(residual))
    bias = regression["bias"]
    maximum = max(row["high"] for row in bias)
    for row in bias:
        members = [r for r, p in zip(residual, predicted) if in_bin(p, row["low"], row["high"], maximum)]
        correct = len(members) == row["n"]
        if len(members) > 1:
            correct = correct and close(row["mean_residual"], sum(members) / len(members)) and close(
                row["mean_absolute_error"], sum(abs(r) for r in members) / len(members))
        else:
            correct = correct and row["mean_residual"] is None and row["mean_absolute_error"] is None
        check(prefix + ": aggregate bias with singleton suppression", correct)


def row_checks(page, oracle, check, prefix):
    view, exported = payload(page, "axr-predictions-payload"), payload(page, "axr-data-payload")
    rows = {row["row_key"]: row for row in exported["rows"] if row["partition"] == "evaluation" and row["retained"]}
    for model in view["models"]:
        model_id = model["model_id"]
        source = oracle["prediction_source"][model_id]
        cases = model["cases"]
        check(prefix + f":{model_id}: exactly exported evaluation sample", {row["row_key"] for row in cases} == set(rows))
        for row in cases:
            raw = rows[row["row_key"]]
            i = raw["processed_position"] - 1
            correct = all(row[key] == raw[key] for key in ["source", "source_row", "processed_position"])
            correct = correct and row["observed"] == source["observed"][i]
            if oracle["task"] == "regression":
                correct = correct and close(row["predicted"], source["prediction"][i])
            else:
                predictions, _ = predicted_classes(source)
                correct = correct and row["predicted"] == predictions[i]
                probability = source["prediction"][i]
                for field, label in [("observed_probability", row["observed"]), ("predicted_probability", row["predicted"])]:
                    expected = probability[source["class_levels"].index(label)] if isinstance(probability, list) else (
                        probability if label == source["positive"] else 1 - probability)
                    correct = correct and close(row[field], expected)
            check(prefix + f":{model_id}: source-aligned case {row['row_key']}", correct)
        panel = select_model(page, model_id)
        disclosure = panel.locator(".prediction-records")
        disclosure.locator("summary").click()
        if oracle["task"] == "regression":
            ordered = sorted(cases, key=lambda row: -abs(row["residual"]))
        else:
            ordered = sorted(cases, key=lambda row: (row["observed"] == row["predicted"], row["observed_probability"]))
        actual = disclosure.locator("[data-case-row]").evaluate_all("nodes=>nodes.map(row=>row.dataset.caseRow)")
        check(prefix + f":{model_id}: source table error ordering", actual == [row["row_key"] for row in ordered[:10]])
        if actual:
            link = disclosure.locator("[data-select-row]").first
            key = link.get_attribute("data-select-row")
            link.click()
            settled(page)
            check(prefix + f":{model_id}: case opens exact source record", page.locator("#data").is_visible()
                  and page.evaluate("AutoXplainRData.getState().selected") == key)
            page.locator('[data-page-link="evaluation"]').click()
            settled(page)
        if oracle["task"] == "binary":
            binary_case_cutoff_checks(page, model_id, source, rows, check, prefix)


def binary_case_cutoff_checks(page, model_id, source, exported_rows, check, prefix):
    """Reconstruct case decisions from original probabilities, never report cases."""
    panel = select_model(page, model_id)
    negative = next(label for label in source["class_levels"] if label != source["positive"])
    for index in [0, 50, 57, 100]:
        cutoff = index / 100
        panel.locator('[data-prediction-cutoff]').evaluate("""(slider, index) => {
            slider.value = String(index); slider.dispatchEvent(new Event('input', {bubbles:true}));
        }""", index)
        expected = []
        for key, row in exported_rows.items():
            position = row["processed_position"] - 1
            observed, probability = source["observed"][position], source["prediction"][position]
            predicted = source["positive"] if probability >= cutoff else negative
            observed_probability = probability if observed == source["positive"] else 1 - probability
            predicted_probability = probability if predicted == source["positive"] else 1 - probability
            expected.append({"key": key, "source": f"{row['source']} {row['source_row']}",
                             "observed": observed, "predicted": predicted,
                             "observed_probability": observed_probability,
                             "predicted_probability": predicted_probability})
        expected.sort(key=lambda row: (row["observed"] == row["predicted"], row["observed_probability"]))
        expected = expected[:10]
        actual = panel.locator('[data-case-row]').evaluate_all("""nodes => nodes.map(row => ({
            key:row.dataset.caseRow, link:row.querySelector('[data-select-row]').dataset.selectRow,
            cells:[...row.cells].map(cell=>cell.textContent)}))""")
        label = f"{prefix}:{model_id}: case cutoff {cutoff:.2f}"
        check(label + " independent mistake ordering", [row["key"] for row in actual] == [row["key"] for row in expected])
        correct = len(actual) == len(expected)
        for rendered, original in zip(actual, expected):
            cells = rendered["cells"]
            correct = correct and rendered["key"] == rendered["link"] == original["key"]
            correct = correct and cells[:3] == [original["source"], original["observed"], original["predicted"]]
            correct = correct and close(float(cells[3]), original["observed_probability"], 5e-5)
            correct = correct and close(float(cells[4]), original["predicted_probability"], 5e-5)
        check(label + " classes probabilities and source keys match", correct)
        check(label + " caption discloses active cutoff", f"cutoff {cutoff:.2f}" in
              panel.locator('.prediction-cases caption').text_content())
    model_ids = page.locator('#prediction-model-select option').evaluate_all("nodes=>nodes.map(node=>node.value)")
    other = next((value for value in model_ids if value != model_id), None)
    if other:
        select_model(page, other)
        panel = select_model(page, model_id)
        check(prefix + f":{model_id}: model switch preserves its own cutoff and case scope",
              panel.locator('[data-prediction-cutoff]').input_value() == "100" and
              panel.locator('[data-case-row]').evaluate_all("nodes=>nodes.map(row=>row.dataset.caseRow)") ==
              [row["key"] for row in expected])
    link = panel.locator('[data-select-row]').first
    if link.count():
        key = link.get_attribute('data-select-row')
        link.click()
        settled(page)
        check(prefix + f":{model_id}: changed-cutoff case opens exact source record",
              page.locator('#data').is_visible() and page.evaluate('AutoXplainRData.getState().selected') == key)
        page.locator('[data-page-link="evaluation"]').click()
        settled(page)


def wrong_model_mutation(browser, case_dir, output, oracle, check):
    html = (case_dir / "binary.html").read_text()
    view = read_json_payload(html, "axr-predictions-payload")
    pair = next(((a, b, i) for a in oracle["prediction_source"] for b in oracle["prediction_source"] if a != b
                 for i in range(1, 100) if counts_at(oracle["prediction_source"][a], i / 100) !=
                 counts_at(oracle["prediction_source"][b], i / 100)), None)
    if pair is None:
        raise RuntimeError("Mutation fixture needs two models with different cutoff counts")
    target, foreign, index = pair
    models = {model["model_id"]: model for model in view["models"]}
    models[target]["cutoffs"] = copy.deepcopy(models[foreign]["cutoffs"])
    altered = replace_json_payload(html, "axr-predictions-payload", view)
    path = output / "wrong-cutoff-model.html"
    path.write_text(altered)
    page = browser.new_page(viewport={"width": 1440, "height": 1000})
    errors, records = [], []
    page.on("pageerror", lambda error: errors.append(str(error)))
    page.goto(path.as_uri())
    page.locator('[data-page-link="evaluation"]').click()
    cutoff_checks(page, oracle, lambda name, passed, evidence=None: records.append(
        {"name": name, "passed": passed, "evidence": evidence}), "wrong-model", indices=[index])
    expected_name = f"wrong-model:{target}: cutoff {index / 100:.2f} counts match independent predictions"
    failure = next((record for record in records if record["name"] == expected_name and not record["passed"]), None)
    check("mutation: wrong model cutoff table rejected by independent numeric counts", failure is not None and not errors,
          {"target": target, "foreign": foreign, "failure": failure, "browser_errors": errors})
    page.close()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--case-dir", type=Path, required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--axe-path", type=Path, required=True)
    parser.add_argument("--cases", nargs="+", default=["regression", "binary", "multiclass", "quick"])
    parser.add_argument("--skip-mutation", action="store_true", help="Skip the deliberate wrong-model cutoff proof")
    args = parser.parse_args()
    folder, output = args.case_dir.resolve(), args.output_dir.resolve()
    output.mkdir(parents=True, exist_ok=True)
    checks, errors, accessibility = [], [], []
    runtime = {"python": platform.python_version(), "playwright": version("playwright")}
    sources = {}

    def check(name, passed, evidence=None):
        checks.append({"name": name, "passed": bool(passed), **({"evidence": evidence} if evidence is not None else {})})

    try:
        with sync_playwright() as playwright:
            browser = playwright.chromium.launch(headless=True)
            runtime["chromium"] = browser.version
            page = browser.new_page(viewport={"width": 1440, "height": 1000})
            page.on("pageerror", lambda error: errors.append(str(error)))
            oracles = {}
            for case in args.cases:
                for path in [folder / f"{case}.json", *sorted(folder.glob(f"{case}*.html"))]:
                    sources[path.name] = hashlib.sha256(path.read_bytes()).hexdigest()
                oracle = json.loads((folder / f"{case}.json").read_text())
                if not oracle.get("prediction_source"):
                    raise RuntimeError(f"{case}.json lacks independent prediction_source; regenerate the fixtures")
                oracles[case] = oracle
                page.goto((folder / f"{case}.html").as_uri())
                page.locator('[data-page-link="evaluation"]').click()
                settled(page)
                view = payload(page, "axr-predictions-payload")
                check(case + ": every original model represented", {model["model_id"] for model in view["models"]}
                      == set(oracle["prediction_source"]))
                for model in view["models"]:
                    model_id, source = model["model_id"], oracle["prediction_source"][model["model_id"]]
                    panel = select_model(page, model_id)
                    prefix = f"{case}:{model_id}"
                    check(prefix + ": summary excludes individual predictions", not model.get("cases")
                          and panel.locator("[data-case-row], [data-select-row]").count() == 0)
                    if oracle["task"] == "regression":
                        regression_checks(model, source, check, prefix)
                    else:
                        confusion_checks(panel, source, check, prefix)
                        score = next(row["calibration_error"] for row in oracle["table"] if row["model_id"] == model_id)
                        calibration_checks(panel, model, source, check, prefix, score)
                if oracle["task"] == "binary":
                    cutoff_checks(page, oracle, check, case)
                select_model(page, oracle["primary"])
                for width in [320, 390, 768, 1440]:
                    page.set_viewport_size({"width": width, "height": 1000})
                    settled(page)
                    check(f"{case}: no page overflow at {width}", page.evaluate("document.documentElement.scrollWidth <= innerWidth + 1"))
                page.screenshot(path=str(output / f"{case}-predictions-1440.png"))
                page.add_script_tag(path=str(args.axe_path))
                axe = page.evaluate("async()=>await axe.run('#evaluation',{runOnly:{type:'tag',values:['wcag2a','wcag2aa','wcag21a','wcag21aa']}})")
                runtime["axe_core"] = axe["testEngine"]["version"]
                compact = lambda items: [{"id": item["id"], "nodes": len(item["nodes"])} for item in items]
                accessibility.append({"case": case, "violations": compact(axe["violations"]),
                                      "incomplete": compact(axe["incomplete"]),
                                      "incomplete_details": axe["incomplete"]})
                check(case + ": Predictions WCAG A/AA automated rules", not axe["violations"], compact(axe["violations"]))
                if oracle["task"] == "binary":
                    nojs = browser.new_page(java_script_enabled=False, viewport={"width": 390, "height": 1000})
                    nojs.goto((folder / f"{case}.html").as_uri())
                    check(case + ": no-JS cutoff controls disabled", nojs.locator("[data-prediction-cutoff]:disabled").count()
                          == len(oracle["prediction_source"]))
                    for model_id, source in oracle["prediction_source"].items():
                        confusion_checks(panel_for(nojs, model_id), source, check, case + ":no-JS:" + model_id)
                    nojs.close()
                    for mode in ["summary", "rows", "none"]:
                        report = folder / f"{case}-{mode}.html"
                        page.goto(report.as_uri())
                        page.locator('[data-page-link="evaluation"]').click()
                        settled(page)
                        view = payload(page, "axr-predictions-payload")
                        check(case + f": {mode} export mode retained", view["mode"] == mode)
                        if mode == "rows":
                            row_checks(page, oracle, check, case + ":rows")
                        else:
                            serialized = json.dumps(view)
                            check(case + f": {mode} embeds no case identifiers", all(key not in serialized for key in
                                  ["row_key", "source_row", "processed_position", "observed_probability", "predicted_probability"])
                                  and page.locator("#evaluation [data-case-row], #evaluation [data-select-row]").count() == 0)
                            for model in view["models"]:
                                calibration_checks(select_model(page, model["model_id"]), model,
                                                   oracle["prediction_source"][model["model_id"]], check,
                                                   f"{case}:{mode}:{model['model_id']}", next(row["calibration_error"]
                                                       for row in oracle["table"] if row["model_id"] == model["model_id"]))
            if not args.skip_mutation and "binary" in oracles:
                wrong_model_mutation(browser, folder, output, oracles["binary"], check)
            browser.close()
    except Exception as error:
        errors.append(f"{type(error).__name__}: {error}")
    result = {"runtime": runtime, "source_sha256": sources, "checks": checks, "errors": errors, "accessibility": accessibility}
    (output / "prediction-checks.json").write_text(json.dumps(result, indent=2))
    failures = [record for record in checks if not record["passed"]]
    print(json.dumps({"checks": len(checks), "failed": len(failures), "errors": errors,
                      "first_failures": failures[:8]}, indent=2))
    return 1 if failures or errors else 0


if __name__ == "__main__":
    raise SystemExit(main())
