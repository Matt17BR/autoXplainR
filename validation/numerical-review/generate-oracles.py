"""Generate independent sample-SD references from exact binary64 input values."""

from decimal import Decimal, localcontext
import json
import math
import os
from pathlib import Path


def cases():
    largest = float.fromhex("0x1.fffffffffffffp1023")
    for label, magnitude in (
        ("near_max", largest), ("large", 1e308),
        ("small", 1e-160), ("tiny", 1e-200),
    ):
        step = math.ulp(magnitude)
        for sign_name, sign in (("negative", -1), ("positive", 1)):
            for gap in (1, 3, 10):
                yield (
                    f"{label}_{sign_name}_gap_{gap}",
                    [sign * (magnitude - gap * step), sign * magnitude],
                )
            yield (
                f"{label}_{sign_name}_cluster",
                [sign * (magnitude - gap * step)
                 for gap in (0, 1, 3, 10, 10, 3, 1, 0, 0, 0)],
            )
    yield "representable_boundary", [-largest, -largest, -largest, largest]
    yield "unrepresentable_boundary", [-largest, largest]
    yield "wide_representable", [-1e308, -3e307, 2e307, 7e307]


def main():
    output = Path(os.environ.get(
        "AXR_NUMERICAL_OUTPUT", str(Path.home() / ".cache/autoxplain-numerical-review")
    ))
    output.mkdir(parents=True, exist_ok=True)
    rows = []
    with localcontext() as context:
        context.prec = 1000
        for name, values in cases():
            numbers = [Decimal.from_float(value) for value in values]
            mean = sum(numbers) / len(numbers)
            variance = sum((value - mean) ** 2 for value in numbers) / (len(numbers) - 1)
            deviation = float(variance.sqrt())
            rows.append({
                "case": name,
                "values": values,
                "expected_sd": deviation if math.isfinite(deviation) else None,
                "unrepresentable": not math.isfinite(deviation),
            })
    assert len(rows) == 35
    (output / "oracles.json").write_text(
        json.dumps(rows, indent=2, allow_nan=False) + "\n", encoding="utf-8"
    )
    print(f"Wrote {len(rows)} deterministic SD oracles to {output / 'oracles.json'}")


if __name__ == "__main__":
    main()
