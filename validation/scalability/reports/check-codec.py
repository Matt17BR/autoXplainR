"""Check offline block decoding, compatibility and readable failure behavior."""
import argparse
import base64
import copy
import json
from pathlib import Path
import re
import sys

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))
from report_payload import decode_data_payload, read_json_payload, replace_json_payload
from playwright.sync_api import sync_playwright
from browser_runtime import launch

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--folder", type=Path, required=True)
parser.add_argument("--browsers", nargs="+", default=["chromium"])
parser.add_argument("--output", type=Path)
args = parser.parse_args()
folder = args.folder.resolve()
out = (args.output or folder / "codec").resolve()
out.mkdir(parents=True, exist_ok=True)
html = (folder / "report.html").read_text()
wire = read_json_payload(html, "axr-data-payload")
decoded = decode_data_payload(wire)
source = json.loads((folder / "source.json").read_text())
checks = []


def check(name, passed, evidence=None):
    checks.append(dict(name=name, passed=bool(passed), evidence=evidence))


def replace_wire(value):
    return replace_json_payload(html, "axr-data-payload", value)


mutations = {"legacy": replace_wire(decoded)}
vendor = (Path(__file__).resolve().parents[3] / "inst/report/fflate-0.8.3.js").read_text()
assert vendor in html, "The real report must embed the licensed decoder asset"
mutations["missing-codec"] = html.replace(vendor, "/* Decoder deliberately removed for this test. */", 1)
corrupt = copy.deepcopy(wire)
assert corrupt["profile"]["encoding"] == "zlib-json-v1"
corrupt["profile"]["bytes"] += 1
mutations["incomplete-profile"] = replace_wire(corrupt)
corrupt = copy.deepcopy(wire)
damaged = bytearray(base64.b64decode(corrupt["profile"]["data"]))
damaged[-1] ^= 1  # Keep valid JSON and byte length; only the checksum changes.
corrupt["profile"]["data"] = base64.b64encode(damaged).decode()
mutations["damaged-checksum"] = replace_wire(corrupt)
corrupt = copy.deepcopy(wire)
corrupt["rows"]["raw"]["response"] = {"encoding": "json", "value": [0]}
mutations["incomplete-column"] = replace_wire(corrupt)
for name, document in mutations.items():
    (out / (name + ".html")).write_text(document)

with sync_playwright() as runtime:
    for engine in args.browsers:
        browser = launch(runtime, engine)
        page = browser.new_page()
        errors = []
        page.on("pageerror", lambda error: errors.append(str(error)))
        page.goto((folder / "report.html").as_uri())
        # Compare decoded JS values to independently decoded Python blocks.
        # This also covers actual compressed identity strings and aliases.
        expected = [{"key": row["row_key"], "raw": row["raw"], "processed": row["processed"]}
                    for row in decoded["rows"]]
        actual = page.evaluate("""()=>{
          const payload=JSON.parse(document.getElementById('axr-data-payload').textContent);
          const store=AutoXplainRPayload.dataStore(payload);
          return store.indices.map(i=>({key:store.meta('row_key',i),
            raw:Object.fromEntries(Object.keys(payload.rows.raw).map(name=>[name,store.value(i,name,'raw')])),
            processed:Object.fromEntries(Object.keys(payload.rows.processed).map(name=>[name,store.value(i,name,'processed')]))}));
        }""")
        check(engine + ": every decoded source value and alias matches independent Python zlib", actual == expected)
        check(engine + ": initial model scores render without decoder errors", not errors and page.locator("[data-model-row]").count() > 0)
        original_scores = page.locator("[data-model-row]").all_text_contents()
        page.close()
        for name in mutations:
            page = browser.new_page()
            errors = []
            page.on("pageerror", lambda error: errors.append(str(error)))
            page.goto((out / (name + ".html")).as_uri())
            page.locator('[data-page-link="data"]').click()
            if name == "legacy":
                page.locator('[data-data-view="records"]').click()
                check(engine + ": old row-object reports remain interactive", page.evaluate(
                    "AutoXplainRData.getState().matchingRows") == source["evaluation_rows"] + source["training_rows"])
                page.locator("#data-record-key").fill(decoded["rows"][-1]["row_key"])
                page.locator("#data-record-lookup button").click()
                check(engine + ": old row-object record identities remain selectable", page.evaluate(
                    "AutoXplainRData.getState().selected") == decoded["rows"][-1]["row_key"])
            else:
                if name == "incomplete-column":
                    page.locator('[data-data-view="records"]').click()
                alert = page.locator("[data-decode-error]")
                check(engine + ": " + name + " gives a visible data failure", alert.is_visible())
                check(engine + ": " + name + " offers actual static distribution tables", page.locator(
                    ".data-static details").count() > 0 and not page.locator(".data-workspace").is_visible())
                fallback = page.locator(".data-static details").first
                totals = None
                if fallback.count():
                    fallback.locator("summary").click()
                    totals = fallback.locator("tbody tr").evaluate_all(
                        "rows=>rows.reduce((sum,row)=>sum+Number(row.cells[1].textContent.replaceAll(',',''))+Number(row.cells[2].textContent.replaceAll(',','')),0)")
                check(engine + ": " + name + " fallback preserves full distribution counts",
                      totals == source["evaluation_rows"] + source["training_rows"], totals)
                check(engine + ": " + name + " leaves model scores unchanged",
                      page.locator("[data-model-row]").all_text_contents() == original_scores)
            check(engine + ": " + name + " has no uncaught runtime errors", not errors, errors)
            page.close()
        context = browser.new_context(java_script_enabled=False)
        page = context.new_page()
        page.goto((folder / "report.html").as_uri() + "#data")
        check(engine + ": JavaScript-disabled reports retain model scores and distribution tables",
              page.locator("[data-model-row]").count() > 0 and page.locator(".data-static details").count() > 0)
        context.close()
        browser.close()

result = dict(passed=all(item["passed"] for item in checks), checks=checks)
(out / "checks.json").write_text(json.dumps(result, indent=2) + "\n")
print(json.dumps(dict(passed=result["passed"], checks=len(checks), failures=[x for x in checks if not x["passed"]]), indent=2))
raise SystemExit(0 if result["passed"] else 1)
