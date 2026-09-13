"""Visible duration-axis controls using hand-written SVGs, never R artifacts.

Run with the same Playwright Python and optional BROWSER_EXECUTABLE used by the
existing browser checks. No production chart generator supplies these answers.
"""
import html
import os
import unittest

from playwright.sync_api import sync_playwright
from report_geometry import affine_axis_feasible, calibration, cost_geometry


class DurationGeometryTests(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.playwright = sync_playwright().start()
        options = {'executable_path': os.environ['BROWSER_EXECUTABLE']} if os.environ.get('BROWSER_EXECUTABLE') else {}
        cls.browser = cls.playwright.chromium.launch(**options)

    @classmethod
    def tearDownClass(cls):
        cls.browser.close()
        cls.playwright.stop()

    def setUp(self):
        self.page = self.browser.new_page()
        self.addCleanup(self.page.close)

    def chart(self, labels=('0 ms', '1 s', '2 s'), maximum=2000,
              resource='training_time_ms', declared='duration-ms'):
        attribute = '' if declared is None else f' data-x-format="{html.escape(declared)}"'
        ticks = ''.join(f'<text text-anchor="middle" x="{x}" y="220">{html.escape(text)}</text>'
                        for x, text in zip((50, 150, 250), labels))
        self.page.set_content(f'''<figure class="axr-chart"{attribute}>
          <svg width="600" height="260" viewBox="0 0 600 260">
            <line class="axr-axis" x1="50" y1="200" x2="250" y2="200"/>
            <line class="axr-grid" x1="50" y1="200" x2="250" y2="200"/>
            <line class="axr-grid" x1="50" y1="100" x2="250" y2="100"/>
            <text text-anchor="end" x="42" y="204">0</text>
            <text text-anchor="end" x="42" y="104">1</text>
            {ticks}
            <g data-chart-point="" data-model-id="a" data-value-x="-999" data-value-y="-999">
              <circle class="axr-point" cx="100" cy="150" r="4" style="stroke-width:2.5"/>
            </g>
            <g data-chart-point="" data-model-id="b" data-value-x="-999" data-value-y="-999">
              <circle class="axr-point" cx="200" cy="175" r="4" style="stroke-width:2.5"/>
            </g>
            <text class="axr-model-label" x="300" y="150">Model A</text>
            <text class="axr-model-label" x="300" y="175">Model B</text>
          </svg></figure>''')
        self.figure = self.page.locator('figure')
        self.resource = resource
        self.rows = [dict(model_id='a', model='Model A', rmse=.5, **{resource: maximum / 4}),
                     dict(model_id='b', model='Model B', rmse=.25, **{resource: maximum * 3 / 4})]

    def result(self):
        return cost_geometry(self.figure, self.rows, 'rmse', self.resource, False)

    def test_duration_formats_use_visible_ticks_in_original_units(self):
        cases = [
            (('0 ms', '1 s', '2 s'), 2000, 'training_time_ms', 'duration-ms'),
            (('0 ms', '1 min', '2 min'), 120, 'training_time_s', 'duration-s'),
            (('0 ms', '2 ms', '4 ms'), 4, 'repeated_prediction_ms_per_row', 'duration-ms-per-row'),
            (('0 ms', '1 h', '2 h'), 7200000, 'prediction_time_ms', 'duration-ms'),
        ]
        for labels, maximum, resource, declared in cases:
            with self.subTest(resource=resource, maximum=maximum):
                self.chart(labels, maximum, resource, declared)
                self.assertEqual(self.result(), (True, None))
                # Also cover old callers which only supply the SVG and axis.
                value, unused = calibration(self.figure.locator('svg'), True)
                self.assertEqual(value(100), maximum / 4)

    def test_numeric_axes_and_older_unformatted_duration_html_remain_supported(self):
        for resource in ('model_size_kb', 'training_time_ms'):
            with self.subTest(resource=resource):
                self.chart(('0', '1000', '2000'), resource=resource, declared=None)
                self.assertEqual(self.result(), (True, None))

    def test_visible_wrong_duration_unit_is_rejected(self):
        for labels in (('0 ms', '1 ms', '2 ms'), ('0 ms', '1 KiB', '2 s'),
                       ('0 ms', '1', '2 s'), ('0 ms', '1 s / row', '2 s')):
            with self.subTest(labels=labels):
                self.chart(labels)
                self.assertFalse(self.result()[0])

    def test_inconsistent_middle_tick_unit_is_rejected(self):
        self.chart(('0 ms', '1 ms', '2 s'))
        matched, evidence = self.result()
        self.assertFalse(matched)
        self.assertIn('no common increasing numeric axis', evidence)

    def test_declared_unit_cannot_replace_expected_resource_identity(self):
        for declared in ('duration-s', 'duration-ms-per-row', 'duration-unknown'):
            with self.subTest(declared=declared):
                self.chart(declared=declared)
                self.assertFalse(self.result()[0])

    def test_shifted_visible_point_rejected_even_with_unchanged_raw_attributes(self):
        self.chart()
        self.assertEqual(self.result(), (True, None))
        self.figure.locator('[data-model-id="a"] .axr-point').evaluate('point=>point.setAttribute("cx", "105")')
        matched, evidence = self.result()
        self.assertFalse(matched)
        self.assertIn('no common increasing numeric axis', evidence)

    def test_whole_second_tick_rounding_accepts_fractional_true_axis(self):
        # True ticks are 0, 62.5, 125 s; the middle displayed tick rounds up.
        self.chart(('0 ms', '1 min 3 s', '2 min 5 s'), 125, 'training_time_s', 'duration-s')
        self.assertEqual(self.result(), (True, None))
        self.figure.locator('[data-model-id="a"] .axr-point').evaluate('point=>point.setAttribute("cx", "105")')
        self.assertFalse(self.result()[0])

    def test_interval_axis_requires_common_mapping_and_strict_point_slack(self):
        ticks = [(0, 0, 0, 0), (100, 100, 62.5, 63.5), (200, 200, 124.5, 125.5)]
        points = [(49.98, 50.02, 31.25, 31.25), (149.98, 150.02, 93.75, 93.75)]
        self.assertTrue(affine_axis_feasible(ticks + points))
        self.assertFalse(affine_axis_feasible(ticks + [(54.98, 55.02, 31.25, 31.25)] + points[1:]))
        self.assertFalse(affine_axis_feasible([(0, 0, 1, 1), (100, 100, 0, 0)]))
        self.assertFalse(affine_axis_feasible([(0, 0, 1, 1), (100, 100, 1, 1)]))
        self.assertFalse(affine_axis_feasible([(0, 0, 1, 1), (0, 0, 2, 2)]))
        self.assertFalse(affine_axis_feasible([(0, 0, float('nan'), 1)]))

    def test_wrong_model_missing_label_and_frontier_still_rejected(self):
        mutations = [
            ('[data-model-id="a"]', 'node=>node.setAttribute("data-model-id", "wrong")'),
            ('.axr-model-label', 'node=>node.textContent="Wrong label"'),
            ('[data-model-id="a"] .axr-point', 'node=>node.style.strokeWidth="1"'),
        ]
        for selector, mutation in mutations:
            with self.subTest(selector=selector, mutation=mutation):
                self.chart()
                self.figure.locator(selector).first.evaluate(mutation)
                self.assertFalse(self.result()[0])

    def test_reversed_or_insufficient_visible_ticks_rejected(self):
        for labels in (('2 s', '1 s', '0 ms'), ('0 ms', '0 ms', '0 ms')):
            with self.subTest(labels=labels):
                self.chart(labels)
                self.assertFalse(self.result()[0])


if __name__ == '__main__':
    unittest.main()
