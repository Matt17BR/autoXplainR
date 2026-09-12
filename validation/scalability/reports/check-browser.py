"""Exercise full-row reports through visible controls; keep source oracles separate."""
import argparse
import json
import math
from pathlib import Path
import re
import sys
import time

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))
from report_payload import decode_block
from playwright.sync_api import sync_playwright
from browser_runtime import launch

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--folder", type=Path, required=True)
parser.add_argument("--browsers", nargs="+", default=["chromium"])
parser.add_argument("--output", type=Path)
args = parser.parse_args()
folder = args.folder.resolve()
out = (args.output or folder / "browser").resolve()
out.mkdir(parents=True, exist_ok=True)
source = json.loads((folder / "source.json").read_text())
html = (folder / "report.html").read_text()
wire = json.loads(re.search(r'<script[^>]*id="axr-data-payload"[^>]*>(.*?)</script>', html, re.S)[1])
keys = decode_block(wire["rows"]["meta"]["row_key"])
partitions = decode_block(wire["rows"]["meta"]["partition"])
positions = decode_block(wire["rows"]["meta"]["source_row"])
expected_keys = {positions[i]: key for i, key in enumerate(keys) if partitions[i] == "evaluation"}
predictions = json.loads(re.search(r'<script[^>]*id="axr-predictions-payload"[^>]*>(.*?)</script>', html, re.S)[1])
model = next(item for item in predictions["models"] if item["model_id"] == "linear")
official = dict(n=model["n"], rmse=model["regression"]["metrics"][0],
                density_rows=sum(item["n"] for item in model["regression"]["density"]),
                residual_rows=sum(item["n"] for item in model["regression"]["residual_histogram"]))
del keys, partitions, positions, wire, html, predictions, model
checks, observations = [], []


def check(name, passed, evidence=None):
    checks.append(dict(name=name, passed=bool(passed), evidence=evidence))


def settled(page):
    page.evaluate("()=>new Promise(r=>requestAnimationFrame(()=>requestAnimationFrame(r)))")


def timed(action, page):
    start = time.perf_counter()
    action()
    settled(page)
    return time.perf_counter() - start


check("Official RMSE agrees with predictions computed independently in R",
      math.isclose(official["rmse"], source["rmse"], rel_tol=1e-13, abs_tol=1e-15))
check("Prediction summaries retain every evaluation row",
      official["n"] == official["density_rows"] == official["residual_rows"] == source["evaluation_rows"])


with sync_playwright() as runtime:
    for engine in args.browsers:
        browser = launch(runtime, engine)
        for width in (1440, 390):
            name = f"{engine}/{width}"
            page = browser.new_page(viewport={"width": width, "height": 950})
            errors, requests = [], []
            page.on("pageerror", lambda error: errors.append(str(error)))
            page.on("request", lambda request: requests.append(request.url) if request.url.startswith(("http:", "https:")) else None)
            metrics = {"browser": engine, "version": browser.version, "width": width}
            metrics["load_seconds"] = timed(lambda: page.goto((folder / "report.html").as_uri()), page)
            check(name + ": full scores visible before opening records", page.locator("[data-model-row]").count() > 0)
            original_scores = page.locator("[data-model-row]").all_text_contents()
            check(name + ": row values decode lazily", page.evaluate("AutoXplainRData.loadedColumns()") == {"raw": [], "processed": []})
            metrics["open_data_seconds"] = timed(lambda: page.locator('[data-page-link="data"]').click(), page)
            check(name + ": distributions do not decode row columns", page.evaluate("AutoXplainRData.loadedColumns()") == {"raw": [], "processed": []})
            metrics["open_records_seconds"] = timed(lambda: page.locator('[data-data-view="records"]').click(), page)
            check(name + ": initial sort control matches the visible source-order table",
                  page.locator("#data-row-sort").input_value() == "source_row" and
                  page.locator("#data-row-table tbody tr").first.locator("td").first.inner_text().endswith(":1"))
            check(name + ": closed scatter creates no points", page.locator("#data-row-scatter circle").count() == 0)
            check(name + ": complete requested export retained", page.evaluate("AutoXplainRData.getState().matchingRows") == source["training_rows"] + source["evaluation_rows"])
            for record in source["selected"]:
                key = expected_keys[record["source_row"]]
                page.locator("#data-record-key").fill(key)
                elapsed = timed(lambda: page.locator('#data-record-lookup button').click(), page)
                metrics.setdefault("source_lookup_seconds", []).append(elapsed)
                check(name + f": exact source identity {key}", page.evaluate("AutoXplainRData.getState().selected") == key)
                actual = page.locator("#data-selected-row tbody tr").evaluate_all("rows=>Object.fromEntries(rows.map(row=>[row.cells[0].textContent,row.cells[1].textContent]))")
                for column, expected in record["values"].items():
                    visible = float(actual[column].replace(",", ""))
                    check(name + f": visible source value {key}/{column}", math.isclose(visible, expected, rel_tol=1e-14, abs_tol=1e-15), [visible, expected])
            page.locator("#data-record-key").fill(expected_keys[1])
            page.locator('#data-record-lookup button').click()
            metrics["next_page_seconds"] = timed(lambda: page.locator("#data-row-next").click(), page)
            check(name + ": paging reaches actual next rows", "11–20" in page.locator("#data-row-page").inner_text())
            page.select_option("#data-split", "both")
            page.select_option("#data-filter-column", "x")
            page.select_option("#data-filter-op", "ge")
            page.locator("#data-filter-value").fill("2")
            metrics["filter_seconds"] = timed(lambda: page.locator('#data-filter-form button[type="submit"]').click(), page)
            expected_count = source["filtered"]["n_evaluation"] + source["filtered"]["n_training"]
            check(name + ": filter counts every exported row", page.evaluate("AutoXplainRData.getState().matchingRows") == expected_count)
            page.locator("#data-filter-reset").click()
            if page.locator("#data-column-select").is_visible():
                page.select_option("#data-column-select", "x")
            else:
                page.locator('[data-column-name="x"]').click()
            page.select_option("#data-y", "z")
            metrics["scatter_seconds"] = timed(lambda: page.locator("#data-scatter-details > summary").click(), page)
            count = page.locator("#data-row-scatter circle[data-row-key]").count()
            full = source["training_rows"] + source["evaluation_rows"]
            check(name + ": bounded scatter discloses displayed and full counts", count == min(1500, full) and
                  f"{count:,} displayed of {full:,}" in page.locator("#data-row-scatter").inner_text())
            check(name + ": full export note uses readable exact counts and accurate scope",
                  f"{full:,} of {full:,}" in page.locator("#data-sample-note").inner_text() and
                  "All source rows exported" in page.locator("#data-sample-note").inner_text())
            check(name + ": scatter sampling leaves official scores unchanged", page.locator("[data-model-row]").all_text_contents() == original_scores)
            last = next(item for item in source["selected"] if item["source_row"] == source["evaluation_rows"])
            geometry = page.locator("#data-row-scatter svg").evaluate("""(svg,key)=>{
              const lines=Array.from(svg.querySelectorAll('line'));
              const point=Array.from(svg.querySelectorAll('circle')).find(item=>item.dataset.rowKey===key);
              return {left:Number(lines[0].getAttribute('x1')),right:Number(lines[0].getAttribute('x2')),
                top:Math.min(...lines.map(item=>Number(item.getAttribute('y1')))),
                bottom:Math.max(...lines.map(item=>Number(item.getAttribute('y1')))),
                cx:Number(point.getAttribute('cx')),cy:Number(point.getAttribute('cy'))};
            }""", expected_keys[source["evaluation_rows"]])
            x, y = source["all_extents"]["x"], source["all_extents"]["z"]
            expected_x = geometry["left"] + (last["values"]["x"] - x[0]) / (x[1] - x[0]) * (geometry["right"] - geometry["left"])
            expected_y = geometry["bottom"] - (last["values"]["z"] - y[0]) / (y[1] - y[0]) * (geometry["bottom"] - geometry["top"])
            check(name + ": scatter axes retain full-data extents, including unplotted extremes",
                  abs(expected_x - geometry["cx"]) < 1e-6 and abs(expected_y - geometry["cy"]) < 1e-6)
            check(name + ": no horizontal page overflow", page.evaluate("document.documentElement.scrollWidth<=innerWidth"))
            page.locator("#data-record-key").fill(expected_keys[source["evaluation_rows"]])
            page.locator('#data-record-lookup button').click()
            check(name + ": last record remains selectable after sampling", page.evaluate("AutoXplainRData.getState().selected") == expected_keys[source["evaluation_rows"]])
            if engine == "chromium":
                session = page.context.new_cdp_session(page)
                session.send("Performance.enable")
                values = session.send("Performance.getMetrics")["metrics"]
                metrics["observed_js_heap_bytes"] = next(item["value"] for item in values if item["name"] == "JSHeapUsedSize")
                session.detach()
                if width == 1440:
                    page.pdf(path=str(out / f"{engine}-records.pdf"), format="A4", print_background=True)
            page.screenshot(path=str(out / f"{engine}-{width}-records.png"), full_page=True)
            check(name + ": offline interaction has no network dependency", not requests, requests)
            check(name + ": no runtime or data-decoder failure", not errors and not page.locator("[data-decode-error]").count(), errors)
            observations.append(metrics)
            page.close()
        browser.close()
result = dict(checks=checks, observations=observations, passed=all(item["passed"] for item in checks),
              memory_scope="Observed V8 JavaScript heap after tasks, not peak browser RSS or total machine memory.")
(out / "checks.json").write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps(dict(passed=result["passed"], checks=len(checks), failures=[x for x in checks if not x["passed"]]), indent=2))
raise SystemExit(0 if result["passed"] else 1)
