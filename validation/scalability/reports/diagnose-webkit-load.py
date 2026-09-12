"""Bound initial large-file loads and separate parser events from report initialization."""
import argparse
import json
from pathlib import Path
import time
from playwright.sync_api import sync_playwright, TimeoutError as PlaywrightTimeout
from browser_runtime import launch

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("--reports", type=Path, nargs="+", required=True)
parser.add_argument("--output", type=Path, required=True)
parser.add_argument("--timeout-ms", type=int, default=15000)
args = parser.parse_args()
results = []
with sync_playwright() as runtime:
    for file in args.reports:
        for enabled in (False, True):
            browser = launch(runtime, "webkit")
            page = browser.new_page(java_script_enabled=enabled)
            started = time.perf_counter()
            events, errors, console = [], [], []
            page.on("domcontentloaded", lambda _: events.append(dict(event="domcontentloaded", seconds=time.perf_counter()-started)))
            page.on("load", lambda _: events.append(dict(event="load", seconds=time.perf_counter()-started)))
            page.on("pageerror", lambda error: errors.append(str(error)))
            page.on("console", lambda message: console.append(dict(text=message.text, seconds=time.perf_counter()-started)))
            status = "loaded"
            try:
                page.goto(file.resolve().as_uri(), wait_until="load", timeout=args.timeout_ms)
            except PlaywrightTimeout:
                status = "initial_navigation_timeout"
            elapsed = time.perf_counter()-started
            result = dict(file=str(file), html_bytes=file.stat().st_size, javascript=enabled,
                          browser=browser.version, timeout_ms=args.timeout_ms, status=status,
                          elapsed_seconds=elapsed, events=events, errors=errors, console=console,
                          runtime_scope="Pinned Playwright WebKit WPE port using isolated Ubuntu24 compatibility libraries on Ubuntu26. This is not a Safari or physical-device timing.")
            # Never query a busy main thread after a navigation deadline.
            if status == "loaded" and enabled:
                result["initialized"] = page.evaluate("({data:!!window.AutoXplainRData, report:!!window.AutoXplainRReport})")
            results.append(result)
            print(json.dumps(result), flush=True)
            browser.close()
            args.output.parent.mkdir(parents=True, exist_ok=True)
            args.output.write_text(json.dumps(results, indent=2)+"\n")
