"""Check score/cost geometry against literal values, including the Pareto steps."""
import json
import math
import os
from pathlib import Path
from playwright.sync_api import sync_playwright

folder = Path(os.environ.get("AXR_STRESS_REPORTS", "~/.cache/autoxplain-stress-0.6.2/reports")).expanduser() / "score-scale"
checks = []


def check(name, passed, details=None):
    checks.append({"name": name, "passed": bool(passed), "details": details})


def settle(page):
    page.evaluate("() => new Promise(resolve => requestAnimationFrame(() => requestAnimationFrame(resolve)))")


def geometry(figure, logarithmic, values=(100, 10, 1)):
    ticks = figure.locator("svg text").evaluate_all("""nodes => nodes.filter(node =>
      node.getAttribute('text-anchor')==='end' && !node.classList.contains('axr-model-label') &&
      Number.isFinite(Number(node.textContent))).map(node =>
      ({value:Number(node.textContent), pixel:Number(node.getAttribute('y'))-4}))""")
    if len({tick["value"] for tick in ticks}) < 2:
        return False, {"ticks": ticks}
    low, high = min(ticks, key=lambda tick: tick["value"]), max(ticks, key=lambda tick: tick["value"])
    transform = math.log10 if logarithmic else lambda value: value
    slope = (transform(high["value"])-transform(low["value"]))/ (high["pixel"]-low["pixel"])
    y_pixel = lambda value: low["pixel"]+(transform(value)-transform(low["value"]))/slope
    points = figure.locator("svg [data-chart-point]").evaluate_all("nodes=>nodes.map(node=>({id:node.dataset.modelId,x:+node.querySelector('.axr-point').getAttribute('cx'),y:+node.querySelector('.axr-point').getAttribute('cy')}))")
    actual = {point["id"]: point for point in points}
    expected = dict(zip(("compact", "middle", "large"), values))
    valid = set(actual) == set(expected) and all(abs(actual[key]["y"]-y_pixel(value)) < .05 for key, value in expected.items())
    frontier = figure.locator("svg .axr-frontier").evaluate("line => [...line.points].map(point=>({x:point.x,y:point.y}))")
    vertices = [("compact", values[0]), ("middle", values[0]), ("middle", values[1]), ("large", values[1]), ("large", values[2])]
    valid &= len(frontier) == len(vertices)
    valid &= all(key in actual and abs(vertex["x"]-actual[key]["x"]) < .05 and abs(vertex["y"]-y_pixel(value)) < .05
                 for vertex, (key, value) in zip(frontier, vertices))
    return valid, {"ticks": ticks, "points": points, "frontier": frontier}


with sync_playwright() as p:
    browser = p.chromium.launch()
    for width in (1440, 390, 320):
        page = browser.new_page(viewport={"width": width, "height": 1000})
        errors = []
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.goto((folder / "scores.html").as_uri())
        settle(page)
        control = page.locator("#score-scale-select")
        figure = page.locator("[data-cost-plot]:visible .axr-chart")
        table = figure.locator("tbody").inner_text()
        check(f"{width}/linear-default", control.input_value() == "linear")
        valid, evidence = geometry(figure, False)
        check(f"{width}/literal-linear-projection", valid, evidence)
        control.focus()
        page.keyboard.press("End")
        page.keyboard.press("Enter")
        settle(page)
        check(f"{width}/keyboard-log", control.input_value() == "log")
        valid, evidence = geometry(figure, True)
        check(f"{width}/literal-log-points-and-frontier", valid, evidence)
        label = figure.locator("svg > text").first
        check(f"{width}/visible-log-label", "log scale" in label.text_content() and label.is_visible())
        check(f"{width}/raw-values-unchanged", figure.locator("tbody").inner_text() == table)
        check(f"{width}/all-models-remain", len(evidence.get("points", [])) == 3)
        page.select_option("#cost-scale-select", "log")
        settle(page)
        valid, evidence = geometry(figure, True)
        check(f"{width}/both-log-axes-frontier", valid, evidence)
        check(f"{width}/url-retains-both-scales", "scoreScale=log" in page.url and "costScale=log" in page.url)
        page.reload()
        settle(page)
        check(f"{width}/fresh-url-restores-log", control.input_value() == "log")
        valid, evidence = geometry(figure, True)
        check(f"{width}/restored-points", valid, evidence)
        # A superficially valid chart with displaced data must fail the oracle.
        dot = figure.locator('svg [data-model-id="middle"] .axr-point')
        dot.evaluate("node => node.setAttribute('cy',+node.getAttribute('cy')+8)")
        valid, evidence = geometry(figure, True)
        check(f"{width}/wrong-position-rejected", not valid, evidence)
        page.reload()
        settle(page)
        label = figure.locator("svg > text").first
        label.evaluate("node=>node.textContent='RMSE'")
        check(f"{width}/unlabelled-log-rejected", "log scale" not in label.text_content())
        page.reload()
        settle(page)
        page.emulate_media(media="print")
        page.evaluate("window.dispatchEvent(new Event('beforeprint'))")
        settle(page)
        valid, evidence = geometry(figure, True)
        check(f"{width}/print-keeps-log-geometry", valid, evidence)
        check(f"{width}/print-names-score-scale", page.locator("#score-scale-select + .print-selection").inner_text() == "Log")
        page.emulate_media(media="screen")
        page.evaluate("window.dispatchEvent(new Event('afterprint'))")
        page.select_option("#metric-select", "log_loss")
        settle(page)
        valid, evidence = geometry(figure, True, (.1, .01, .001))
        check(f"{width}/small-positive-log-loss", valid, evidence)
        for metric, phrase in (("mae", "zero or negative"), ("r_squared", "linear axis"), ("accuracy", "linear axis")):
            page.select_option("#metric-select", metric)
            settle(page)
            check(f"{width}/{metric}-rejects-log", control.input_value() == "linear" and
                  control.locator('option[value="log"]').evaluate("node=>node.disabled") and
                  phrase in page.locator("#score-scale-note").inner_text())
        page.select_option("#metric-select", "rmse")
        control.select_option("log")
        settle(page)
        check(f"{width}/no-horizontal-overflow", page.evaluate("document.documentElement.scrollWidth<=innerWidth"))
        page.locator(".comparison-chart").screenshot(path=str(folder / f"log-score-{width}.png"))
        check(f"{width}/runtime", not errors, errors)
        page.close()
    # The actual unstable alternative must stay visible while the useful models
    # become easier to distinguish on an explicitly selected log score axis.
    path = folder.parent / "sparse_wide-core" / "summary.html"
    if path.exists():
        page = browser.new_page(viewport={"width": 1440, "height": 1000})
        page.goto(path.as_uri())
        settle(page)
        figure = page.locator("[data-cost-plot]:visible .axr-chart")
        get_points = lambda: figure.locator("svg [data-chart-point]").evaluate_all("nodes=>nodes.map(node=>({id:node.dataset.modelId,y:+node.querySelector('.axr-point').getAttribute('cy')}))")
        before = {point["id"]: point["y"] for point in get_points()}
        table = page.locator(".model-table").inner_text()
        page.select_option("#score-scale-select", "log")
        settle(page)
        after = {point["id"]: point["y"] for point in get_points()}
        check("sparse/all-models-preserved", set(before) == set(after) and len(after) == 4 and "linear_model" in after)
        check("sparse/raw-scores-preserved", page.locator(".model-table").inner_text() == table and "110.5" in table)
        gap_before = abs(before["main_model"]-before["simple_baseline"])
        gap_after = abs(after["main_model"]-after["simple_baseline"])
        check("sparse/useful-score-separation-improves", gap_after > 3*gap_before,
              {"linear_pixels": gap_before, "log_pixels": gap_after})
        page.screenshot(path=str(folder / "sparse-log-score-desktop.png"), full_page=True)
        page.close()
    browser.close()
summary = {"passed": sum(item["passed"] for item in checks), "failed": sum(not item["passed"] for item in checks), "checks": checks}
(folder / "verification.json").write_text(json.dumps(summary, indent=2)+"\n")
print(summary["passed"], "passed", summary["failed"], "failed")
for item in checks:
    if not item["passed"]:
        print(item)
if summary["failed"]:
    raise SystemExit(1)
