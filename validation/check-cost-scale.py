"""Check cost scale controls against literal values, visible ticks, and fresh loads."""
import argparse
import json
import math
import os
from pathlib import Path

from playwright.sync_api import expect, sync_playwright
from report_geometry import calibration, points, label_collisions

parser = argparse.ArgumentParser()
parser.add_argument("--case-dir", type=Path, required=True)
parser.add_argument("--output-dir", type=Path, required=True)
args = parser.parse_args()
args.output_dir.mkdir(parents=True, exist_ok=True)
url = (args.case_dir / "cost-scale-oracle.html").resolve().as_uri()
checks = []


def check(label, passed, evidence=None):
    checks.append(dict(check=label, passed=bool(passed), evidence=evidence))


def settled(page):
    page.evaluate("()=>new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)))")


def accessible_description_is(control, text):
    try:
        expect(control).to_have_accessible_description(text, timeout=1000)
        return True
    except AssertionError:
        return False


def geometry(figure, log=False, higher=False):
    svg = figure.locator("svg")
    labels = svg.evaluate("""svg=>{
      const bottom=+svg.querySelector('.axr-axis').getAttribute('y1');
      return [...svg.querySelectorAll('text')].filter(t=>t.getAttribute('text-anchor')==='middle' &&
        +t.getAttribute('y')>bottom && +t.getAttribute('y')<bottom+30 && Number.isFinite(Number(t.textContent)))
        .map(t=>({value:Number(t.textContent),pixel:+t.getAttribute('x')}));
    }""")
    if len({x["value"] for x in labels}) < 2:
        return False, "Not enough distinct visible cost ticks"
    first, last = min(labels, key=lambda x: x["value"]), max(labels, key=lambda x: x["value"])
    transform = math.log10 if log else lambda x: x
    slope = (transform(last["value"]) - transform(first["value"])) / (last["pixel"] - first["pixel"])
    x_pixel = lambda x: first["pixel"] + (transform(x) - transform(first["value"])) / slope
    y_value, y_slope = calibration(svg)
    expected_y = [-.5, .2, .9] if higher else [10, 6, 2]
    expected = dict(zip(["compact", "middle", "large"], zip([1, 10, 100], expected_y)))
    dots = points(svg)
    valid = len(dots) == 3 and {dot["model"] for dot in dots} == set(expected)
    for dot in dots:
        x, y = expected[dot["model"]]
        valid &= abs(dot["x"] - x_pixel(x)) < .04 and abs(y_value(dot["y"]) - y) < abs(y_slope) * .04
        valid &= dot["frontier"]
    actual = figure.locator(".axr-frontier").evaluate("line=>[...line.points].map(p=>[p.x,p.y])")
    vertices = [(1, expected_y[0]), (10, expected_y[0]), (10, expected_y[1]),
                (100, expected_y[1]), (100, expected_y[2])]
    valid &= len(actual) == len(vertices)
    valid &= all(abs(px - x_pixel(x)) < .04 and abs(y_value(py) - y) < abs(y_slope) * .04
                 for (px, py), (x, y) in zip(actual, vertices))
    valid &= not label_collisions(svg)
    ordered = sorted(dots, key=lambda dot: dot["x"])
    gaps = [ordered[i + 1]["x"] - ordered[i]["x"] for i in range(2)]
    valid &= math.isclose(gaps[1] / gaps[0], 1 if log else 10, rel_tol=1e-6)
    return bool(valid), dict(ticks=labels, points=dots, gaps=gaps, vertices=actual)


with sync_playwright() as p:
    browser = p.chromium.launch(**({"executable_path": os.environ["BROWSER_EXECUTABLE"]}
                                 if os.environ.get("BROWSER_EXECUTABLE") else {}))
    for width in [1440, 390, 320]:
        page = browser.new_page(viewport=dict(width=width, height=1000))
        errors = []
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.goto(url)
        settled(page)
        control = page.locator("#cost-scale-select")
        log_disabled = lambda: control.locator('option[value="log"]').evaluate("option=>option.disabled")
        figure = page.locator("[data-cost-plot]:visible .axr-chart")
        table = figure.locator("tbody").inner_text()
        check(f"linear default {width}", control.input_value() == "linear")
        check(f"positive costs have no false accessible warning {width}",
              accessible_description_is(control, ""))
        valid, evidence = geometry(figure)
        check(f"literal linear geometry {width}", valid, evidence)
        control.focus()
        page.keyboard.press("End")
        page.keyboard.press("Enter")
        settled(page)
        check(f"keyboard chooses log {width}", control.input_value() == "log")
        valid, evidence = geometry(figure, log=True)
        check(f"equal cost ratios and frontier steps {width}", valid, evidence)
        check(f"log axis is explicitly labelled {width}", "log scale" in figure.locator("svg").text_content())
        check(f"original table unchanged {width}", figure.locator("tbody").inner_text() == table)
        for model, value in [("compact", "1"), ("middle", "10"), ("large", "100")]:
            figure.locator(f'[data-model-id="{model}"]').focus()
            check(f"raw cost remains inspectable {width} {model}",
                  f"R object size (KiB): {value};" in figure.locator(".axr-chart-detail").inner_text())
        check(f"chosen scale persists in hash {width}", "costScale=log" in page.url)
        page.reload()
        settled(page)
        check(f"fresh load restores log {width}", control.input_value() == "log")
        valid, evidence = geometry(figure, log=True)
        check(f"restored log geometry {width}", valid, evidence)
        page.select_option("#metric-select", "r_squared")
        settled(page)
        valid, evidence = geometry(figure, log=True, higher=True)
        check(f"higher-is-better score preserves cost ratios {width}", valid, evidence)
        page.emulate_media(media="print")
        page.evaluate("window.dispatchEvent(new Event('beforeprint'))")
        settled(page)
        valid, evidence = geometry(figure, log=True, higher=True)
        check(f"print preserves chosen log projection {width}", valid, evidence)
        check(f"print names selected scale {width}", page.locator("#cost-scale-select + .print-selection").inner_text() == "Log")
        page.emulate_media(media="screen")
        page.evaluate("window.dispatchEvent(new Event('afterprint'))")
        settled(page)
        page.select_option("#resource-select", "training_time_ms")
        settled(page)
        scale_state = lambda: dict(value=control.input_value(), disabled=log_disabled(),
                                   note=page.locator("#cost-scale-note").inner_text())
        check(f"zero readings disable log without offset {width}", control.input_value() == "linear" and
              log_disabled() and "timer resolution" in page.locator("#cost-scale-note").inner_text(), scale_state())
        check(f"zero costs expose their reason to assistive technology {width}",
              accessible_description_is(control,
                  "Log scale needs positive costs. A zero timing may be below timer resolution."))
        control.focus()
        page.keyboard.press("End")
        page.keyboard.press("Enter")
        settled(page)
        check(f"keyboard cannot select invalid log option {width}", control.input_value() == "linear")
        check(f"zero rejection resets saved state {width}", "costScale=linear" in page.url)
        page.select_option("#resource-select", "prediction_time_ms")
        settled(page)
        check(f"negative readings disable log {width}", log_disabled() and
              "nonpositive" in page.locator("#cost-scale-note").inner_text(), scale_state())
        page.select_option("#resource-select", "repeated_prediction_ms_per_row")
        settled(page)
        check(f"missing comparison disables scale {width}", control.is_disabled() and
              "no finite comparison" in page.locator("#cost-scale-note").inner_text())
        page.select_option("#resource-select", "model_size_kb")
        control.select_option("log")
        settled(page)
        check(f"positive resource restores usable control {width}", not control.is_disabled())
        check(f"positive costs clear the previous accessible warning {width}",
              accessible_description_is(control, ""))
        bounds = page.evaluate("({width:innerWidth,scroll:document.documentElement.scrollWidth})")
        check(f"no horizontal overflow {width}", bounds["scroll"] <= bounds["width"] + 1, bounds)
        page.locator(".comparison-chart").screenshot(path=str(args.output_dir / f"log-cost-{width}.png"))
        # A chart with a log label but linear point positions must fail the
        # ratio oracle even when scores, names, and source data remain intact.
        figure.locator("svg").evaluate("""svg=>{
          const dots=[...svg.querySelectorAll('.axr-point')];
          const x=dots.map(dot=>+dot.getAttribute('cx')).sort((a,b)=>a-b);
          svg.querySelector('[data-model-id="middle"] .axr-point').setAttribute('cx',x[0]+(x[2]-x[0])*9/99);
        }""")
        valid, evidence = geometry(figure, log=True, higher=True)
        check(f"linear spacing disguised as logarithmic is rejected {width}", not valid, evidence)
        check(f"no runtime errors {width}", not errors, errors)
        page.close()
        page = browser.new_page(viewport=dict(width=width, height=1000), java_script_enabled=False)
        page.goto(url + "#overview?costScale=log")
        check(f"noJS control is disabled and truthful {width}", page.locator("#cost-scale-select").is_disabled() and
              "Linear cost axis" in page.locator("#cost-scale-note").inner_text())
        figure = page.locator('[data-cost-plot="rmse"][data-resource="model_size_kb"] .axr-chart')
        valid, evidence = geometry(figure)
        check(f"noJS retains original linear geometry {width}", valid, evidence)
        page.close()
    browser.close()

(args.output_dir / "cost-scale-checks.json").write_text(json.dumps(checks, indent=2))
failures = [check for check in checks if not check["passed"]]
print(json.dumps(dict(checks=len(checks), failures=failures), indent=2))
raise SystemExit(bool(failures))
