"""Decode report data with Python's zlib, independently of the browser codec."""
import base64
import json
from html import unescape
import re
import zlib


def read_json_text(html, identifier):
    """Read ordinary or ordered inert-script text without executing browser code."""
    main, chunks = [], []
    for match in re.finditer(r'<script\b([^>]*)>(.*?)</script\s*>', html, re.S | re.I):
        attrs = {name.lower(): unescape(value) for name, value in
                 re.findall(r'([\w-]+)="([^"]*)"', match[1])}
        if attrs.get("id") == identifier:
            main.append((attrs, match[2]))
        if attrs.get("data-json-owner") == identifier:
            chunks.append((attrs, match[2]))
    if len(main) != 1:
        raise ValueError("Report JSON identifier is missing or duplicated")
    attrs, body = main[0]
    if "data-json-chunks" not in attrs:
        if chunks:
            raise ValueError("Unexpected report JSON chunks")
        return body
    count = int(attrs["data-json-chunks"])
    if body or count < 1 or len(chunks) != count:
        raise ValueError("Report JSON chunk count mismatch")
    if any(item.get("type") != "application/octet-stream" or
           item.get("data-json-chunk") != str(i + 1)
           for i, (item, _) in enumerate(chunks)):
        raise ValueError("Report JSON chunk order or type mismatch")
    return "".join(text for _, text in chunks)


def read_json_payload(html, identifier):
    return json.loads(read_json_text(html, identifier))


def replace_json_payload(html, identifier, payload):
    """Replace a test payload, removing its old chunks so mutations stay active."""
    read_json_text(html, identifier)  # Reject a malformed source before mutation.
    encoded = json.dumps(payload, ensure_ascii=True).replace("<", r"\u003c").replace(">", r"\u003e").replace("&", r"\u0026")
    def replace(match):
        attrs = {name.lower(): unescape(value) for name, value in
                 re.findall(r'([\w-]+)="([^"]*)"', match[1])}
        if attrs.get("data-json-owner") == identifier:
            return ""
        if attrs.get("id") != identifier:
            return match[0]
        opening = re.sub(r'\sdata-json-chunks="[^"]*"', "", match[1])
        return "<script" + opening + ">" + encoded + "</script>"
    return re.sub(r'<script\b([^>]*)>(.*?)</script\s*>', replace, html, flags=re.S | re.I)


def decode_block(block):
    if block is None or not isinstance(block, dict) or "encoding" not in block:
        return block
    if block["encoding"] == "json":
        return block["value"]
    if block["encoding"] != "zlib-json-v1":
        raise ValueError("Unsupported report block encoding")
    raw = zlib.decompress(base64.b64decode(block["data"]))
    if len(raw) != block["bytes"]:
        raise ValueError("Report block byte count mismatch")
    return json.loads(raw)


def decode_data_payload(payload):
    if payload.get("schema_version") != 2:
        return payload
    result = {key: value for key, value in payload.items() if key != "schema_version"}
    result["profile"] = decode_block(payload["profile"])
    store = payload["rows"]
    if store is None:
        return result
    if store["layout"] != "columns-v1":
        raise ValueError("Unsupported report row layout")
    n = store["length"]
    meta = {key: decode_block(block) for key, block in store["meta"].items()}
    stages = {}
    for stage in ("raw", "processed"):
        source = store[stage]
        if source is None:
            stages[stage] = None
            continue
        stages[stage] = {}
        for name, block in source.items():
            if block["encoding"] == "reference":
                if stage != "processed" or block["stage"] != "raw":
                    raise ValueError("Unsupported column alias")
                values = stages["raw"][block["column"]]
            else:
                values = decode_block(block)
            if len(values) != n:
                raise ValueError("Report column row count mismatch")
            stages[stage][name] = values
    flags = {
        stage: {name: set(indices) for name, indices in (store["nonfinite"][stage] or {}).items()}
        for stage in ("raw", "processed")
    }
    rows = []
    for index in range(n):
        row = {key: values[index] for key, values in meta.items()}
        row["nonfinite"] = {
            stage: [name for name, indices in flags[stage].items() if index + 1 in indices]
            for stage in ("raw", "processed")
        }
        for stage in ("raw", "processed"):
            row[stage] = None if stages[stage] is None else {
                name: values[index] for name, values in stages[stage].items()
            }
        rows.append(row)
    result["rows"] = rows
    return result


def decode_prediction_payload(payload):
    if payload.get("schema_version") != 2:
        return payload
    result = {key: value for key, value in payload.items() if key != "schema_version"}
    models = []
    for model in payload["models"]:
        item = dict(model)
        store = model.get("cases")
        if store is not None:
            if store["layout"] != "case-columns-v1":
                raise ValueError("Unsupported prediction case layout")
            columns = {name: decode_block(block) for name, block in (store["columns"] or {}).items()}
            if any(len(values) != store["length"] for values in columns.values()):
                raise ValueError("Prediction column row count mismatch")
            item["cases"] = [
                {name: values[i] for name, values in columns.items()} for i in range(store["length"])
            ]
        models.append(item)
    result["models"] = models
    return result
