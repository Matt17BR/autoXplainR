"""Read plotted answers from visible axes, independently of R's pixel formulas.

Tables and data attributes can remain correct while a graphic lies. These checks
calibrate SVG coordinates from its labeled ticks, then compare plotted positions
with the retained numerical evidence. No report padding or scale helper is reused.
"""
import math


def on_axis(actual, expected, slope):
    # Compare within 0.01 SVG units. A fixed tolerance in outcome units could
    # accept a completely flat small-probability effect.
    return math.isclose(actual, expected, rel_tol=0, abs_tol=abs(slope) * .01)


def calibration(svg, horizontal=False):
    anchor = 'middle' if horizontal else 'end'
    labels = svg.locator(f'text.tick[text-anchor="{anchor}"]').evaluate_all(
        'nodes => nodes.filter(n => n.textContent !== "zero").map(n => '
        '({text:n.textContent, x:Number(n.getAttribute("x"))}))')
    values = [float(label['text']) for label in labels]
    positions = ([label['x'] for label in labels] if horizontal else
                 svg.locator('.grid-line').evaluate_all('nodes => nodes.map(n => Number(n.getAttribute("y1")))'))
    if len(values) != len(positions) or len(set(values)) < 2:
        raise ValueError('The numeric axis has fewer than two usable labeled ticks')
    lo, hi = min(range(len(values)), key=values.__getitem__), max(range(len(values)), key=values.__getitem__)
    slope = (values[hi] - values[lo]) / (positions[hi] - positions[lo])
    return lambda pixel: values[lo] + (pixel - positions[lo]) * slope, slope


def cost_geometry(plot, rows, metric, resource, higher):
    svg = plot.locator('svg.tradeoff-plot')
    x_value, x_slope = calibration(svg, horizontal=True)
    y_value, slope = calibration(svg)
    labels = plot.locator('.chart-key li').all_text_contents()
    dots = svg.locator('.tradeoff-point').evaluate_all('''nodes => nodes.map(n => ({
      x:Number(n.getAttribute('cx')), y:Number(n.getAttribute('cy')),
      frontier:n.classList.contains('tradeoff-pareto')}))''')
    available = [row for row in rows if row.get(metric) is not None and row.get(resource) is not None]
    if len(dots) != len(available) or sorted(labels) != sorted(row['model'] for row in available):
        return False, 'Plot points or their key omit or duplicate a measured model'
    if (slope < 0) != higher:
        return False, 'The better performance direction does not point upwards'
    for label, dot in zip(labels, dots):
        row = next(row for row in available if row['model'] == label)
        if not on_axis(x_value(dot['x']), row[resource], x_slope) or not on_axis(y_value(dot['y']), row[metric], slope):
            return False, dict(model=label, plotted=[x_value(dot['x']), y_value(dot['y'])],
                               expected=[row[resource], row[metric]])
        loss = -row[metric] if higher else row[metric]
        dominated = any(other[resource] <= row[resource] and
                        (-other[metric] if higher else other[metric]) <= loss and
                        (other[resource] < row[resource] or
                         (-other[metric] if higher else other[metric]) < loss)
                        for other in available)
        if dot['frontier'] == dominated:
            return False, f'The frontier outline is incorrect for {label}'
    return True, None


def effect_geometry(panel, curve):
    svg = panel.locator('svg.effect-plot')
    y_value, slope = calibration(svg)
    if slope >= 0:
        return False, 'Larger effects must appear higher on the effect axis'
    x = next(iter(curve.values()))
    values = curve.get('accumulated_effect', curve.get('partial_dependence'))
    dots = svg.locator('.effect-point').evaluate_all('''nodes => nodes.map(n => [
      Number(n.getAttribute('cx')), Number(n.getAttribute('cy'))])''')
    if len(dots) != len(values):
        return False, 'There must be one plotted estimate for each retained curve row'
    numeric = svg.get_attribute('data-axis-type') == 'numeric'
    if numeric:
        x_value, x_slope = calibration(svg, horizontal=True)
        line = svg.locator('.effect-line')
        joined = line.evaluate('n => [...n.points].map(p => [p.x,p.y])')
        if len(joined) != len(dots) or any(abs(a - b) > .001
                                          for p, q in zip(joined, dots) for a, b in zip(p, q)):
            return False, 'The fitted line does not join the plotted estimates'
    else:
        ticks = svg.locator('text.tick[text-anchor="middle"]').evaluate_all(
            'nodes => nodes.map(n => ({text:n.textContent, x:Number(n.getAttribute("x"))}))')
        if [tick['text'] for tick in ticks] != [str(value) for value in x]:
            return False, 'Categorical axis labels do not match the fitted levels'
    for i, dot in enumerate(dots):
        if not on_axis(y_value(dot[1]), values[i], slope):
            return False, dict(row=i + 1, plotted=y_value(dot[1]), expected=values[i])
        if numeric and not on_axis(x_value(dot[0]), x[i], x_slope):
            return False, f'Input spacing is wrong at curve row {i + 1}'
        if not numeric and abs(dot[0] - ticks[i]['x']) > .001:
            return False, f'Estimate is under the wrong category at curve row {i + 1}'
    return True, None
