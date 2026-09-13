"""Independent checks for original resource values and their readable display."""
from decimal import Decimal
import math
import re

DURATION_UNITS = {
    'training_time_ms': 'ms', 'prediction_time_ms': 'ms',
    'training_time_s': 's', 'repeated_prediction_ms_per_row': 'ms / row',
}
NUMBER = r'(?:\d+(?:\.\d*)?|\.\d+)'
EXACT_NUMBER = re.compile(r'-?' + NUMBER + r'(?:[eE][+-]?\d+)?\Z')


def parse_duration(text, per_row=False):
    """Read visible units into seconds without report data/scale attributes."""
    if not isinstance(text, str):
        raise ValueError('Missing duration text')
    suffix = ' / row'
    if per_row:
        if not text.endswith(suffix):
            raise ValueError('Missing per-row duration unit')
        text = text[:-len(suffix)]
    elif text.endswith(suffix):
        raise ValueError('Unexpected per-row duration unit')
    if text == '~0 ms':
        return 0.0
    sign = -1 if text.startswith('-') else 1
    text = text[1:] if sign < 0 else text
    parts = list(re.finditer('(' + NUMBER + r'(?:[eE][+-]?\d+)?) (h|min|s|ms)', text))
    if not parts or ' '.join(part[0] for part in parts) != text:
        raise ValueError('Invalid duration units or component order')
    units = {'h': 3600, 'min': 60, 's': 1, 'ms': .001}
    order = [list(units).index(part[2]) for part in parts]
    if order != sorted(set(order)):
        raise ValueError('Invalid duration component order')
    value = sum(float(part[1]) * units[part[2]] for part in parts)
    if not math.isfinite(value):
        raise ValueError('Nonfinite duration')
    return sign * value


def raw_matches_oracle(raw, expected):
    """Account only for the independent JSON oracle's 16 significant digits.

    HTML attributes and exact titles use 17-digit binary64 round-trip strings.
    The allowance below is the half-quantum of the oracle serialization plus
    the half-quantum of that 17-digit text, not display or measurement tolerance.
    """
    if not isinstance(raw, str) or not EXACT_NUMBER.fullmatch(raw):
        return False
    value = Decimal(raw)
    answer = expected if isinstance(expected, Decimal) else Decimal(str(expected))
    if not value.is_finite() or not answer.is_finite() or not math.isfinite(float(value)):
        return False
    if answer == 0:
        return value == 0
    oracle_half_quantum = Decimal('0.5').scaleb(answer.adjusted() - 15)
    raw_half_quantum = Decimal(0) if value == 0 else Decimal('0.5').scaleb(value.adjusted() - 16)
    return abs(value - answer) <= oracle_half_quantum + raw_half_quantum


def duration_display_matches(text, value, resource):
    per_row = resource == 'repeated_prediction_ms_per_row'
    try:
        actual = parse_duration(text, per_row)
    except ValueError:
        return False
    seconds = value if resource == 'training_time_s' else value / 1000
    # Readable units can change without changing the represented measurement.
    # Preserve the existing short-value display precision in original units;
    # whole-second long durations may round by at most half a second.
    absolute_precision = 1e-8 if resource == 'training_time_s' else 1e-11
    tolerance = .5 if abs(seconds) >= 60 else max(absolute_precision, abs(seconds) * .0006)
    return math.isfinite(actual) and abs(actual - seconds) <= tolerance


def exact_tooltip_matches(raw, tooltip, unit):
    suffix = ' ' + unit if unit else ''
    if not isinstance(raw, str) or not isinstance(tooltip, str) or (suffix and not tooltip.endswith(suffix)):
        return False
    numeric = tooltip[:-len(suffix)] if suffix else tooltip
    return bool(EXACT_NUMBER.fullmatch(raw) and EXACT_NUMBER.fullmatch(numeric)) and \
        math.isfinite(float(raw)) and float(numeric) == float(raw)


def resource_measurement(resource, expected, raw, tooltip, shown):
    """Check all three representations against independent R answer data."""
    if expected is None:
        checks = {'raw_matches_R': raw == '', 'exact_tooltip': tooltip == 'Unavailable',
                  'readable_display': shown == 'Unavailable'}
    else:
        raw_ok = raw_matches_oracle(raw, expected)
        unit = DURATION_UNITS.get(resource)
        checks = {'raw_matches_R': raw_ok,
                  'exact_tooltip': exact_tooltip_matches(raw, tooltip, unit),
                  'readable_display': False}
        if raw_ok:
            if unit:
                checks['readable_display'] = duration_display_matches(shown, float(raw), resource)
            else:
                try:
                    # Preserve the existing plain-number display precision check.
                    actual = 0 if shown == '~0' else float(shown)
                    answer = float(expected)
                    checks['readable_display'] = math.isfinite(actual) and abs(actual - answer) <= max(1e-8, abs(answer) * .0006)
                except (ValueError, TypeError):
                    pass
    return all(checks.values()), {'resource': resource, 'raw': raw, 'tooltip': tooltip,
                                 'shown': shown, 'expected': None if expected is None else str(expected),
                                 'checks': checks}
