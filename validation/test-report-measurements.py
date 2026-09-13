"""Small independent duration/unit and raw/tooltip counterexamples; no R."""
from decimal import Decimal
import unittest
from report_measurements import parse_duration, resource_measurement


class Measurements(unittest.TestCase):
    def measurement(self, value, shown, resource='training_time_s', raw=None, tooltip=None):
        raw = str(value) if raw is None else raw
        unit = {'training_time_s': 's', 'training_time_ms': 'ms',
                'repeated_prediction_ms_per_row': 'ms / row'}[resource]
        return resource_measurement(resource, Decimal(str(value)), raw,
                                    raw + ' ' + unit if tooltip is None else tooltip, shown)[0]

    def test_valid_equivalent_units_and_rounded_durations(self):
        for value, shown in [(0, '~0 ms'), (.000000001, '0.000001 ms'), (.002, '2 ms'),
                             (.123456, '123.5 ms'), (.99999, '1000 ms'), (1, '1 s'),
                             (1.23456, '1.235 s'), (59.9999, '60 s'), (60, '1 min'),
                             (60.5, '1 min 1 s'), (3599.5, '1 h'), (3601, '1 h 1 s'),
                             (3661, '1 h 1 min 1 s'), (-60.5, '-1 min 1 s'),
                             (.99999, '1 s'), (59.9999, '1 min'), (60.5, '1 min'),
                             (60, '60 s'), (60, '0 h 1 min'), (60, '1 min 0 s'),
                             (3600, '60 min'), (0, '0 ms'), (0, '0 s'),
                             (60.5, '1 min 0.5 s'), (.002, '2e0 ms')]:
            with self.subTest(value=value):
                self.assertTrue(self.measurement(value, shown))
        self.assertTrue(self.measurement(2, '2 ms', 'training_time_ms'))
        self.assertTrue(self.measurement(.25, '0.25 ms / row', 'repeated_prediction_ms_per_row'))

    def test_wrong_units_and_values_rejected(self):
        for value, shown in [(.002, '2 s'), (.002, '3 ms'), (60.6, '1 min'),
                             (3599.4, '1 h'), (1, '-1 s'),
                             (.123456, '123.3 ms')]:
            with self.subTest(value=value, shown=shown):
                self.assertFalse(self.measurement(value, shown))
        self.assertFalse(self.measurement(.25, '0.25 ms', 'repeated_prediction_ms_per_row'))
        self.assertFalse(self.measurement(2, '2 ms / row', 'training_time_ms'))

    def test_raw_and_tooltip_are_independently_bound(self):
        self.assertFalse(self.measurement(2, '2 ms', 'training_time_ms', raw='3'))
        self.assertFalse(self.measurement(2, '2 ms', 'training_time_ms', tooltip='3 ms'))
        self.assertFalse(self.measurement(2, '2 ms', 'training_time_ms', tooltip='2 s'))
        self.assertFalse(self.measurement(2, '2 ms', 'training_time_ms', tooltip='2.0000000000000004 ms'))
        self.assertTrue(self.measurement(2, '2 ms', 'training_time_ms', tooltip='2.0 ms'))
        self.assertFalse(self.measurement(2, '2 ms', 'training_time_ms', raw='2.00000000001'))
        self.assertFalse(self.measurement(2, '2 ms', 'training_time_ms', raw='NaN'))
        self.assertFalse(self.measurement('1e-20', '~0 ms', raw='0'))
        self.assertTrue(self.measurement('1.234567890123457', '1.235 s', raw='1.2345678901234567'))

    def test_unavailable_and_plain_resources(self):
        self.assertTrue(resource_measurement('training_time_ms', None, '', 'Unavailable', 'Unavailable')[0])
        for raw, title, shown in [('0', 'Unavailable', 'Unavailable'), ('', '0 ms', 'Unavailable'), ('', 'Unavailable', '0 ms')]:
            self.assertFalse(resource_measurement('training_time_ms', None, raw, title, shown)[0])
        self.assertTrue(resource_measurement('model_size_kb', Decimal('118.53125'), '118.53125', '118.53125', '118.5')[0])
        self.assertFalse(resource_measurement('model_size_kb', Decimal('118.53125'), '118.53125', '118.53125 ms', '118.5')[0])

    def test_visible_axis_duration_parser(self):
        for text, seconds in [('0 ms', 0), ('250 ms', .25), ('1 min 30 s', 90), ('2 h', 7200)]:
            self.assertEqual(parse_duration(text), seconds)
        self.assertEqual(parse_duration('1 h 60 min'), 7200)
        self.assertEqual(parse_duration('1 min 0 s'), 60)
        for text in ['2ms', '2 MS', '1 s 1 min', 'NaN s']:
            with self.assertRaises(ValueError):
                parse_duration(text)


if __name__ == '__main__':
    unittest.main()
