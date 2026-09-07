(() => {
  'use strict';
  const NS = 'http://www.w3.org/2000/svg';
  const figures = Array.from(document.querySelectorAll('.axr-chart'));
  const retained = new WeakMap();
  const finite = Number.isFinite;
  const number = value => !finite(value) ? 'Unavailable' : value !== 0 && (Math.abs(value) < .001 || Math.abs(value) >= 1e6) ?
    Number(value.toPrecision(6)).toExponential() : Number(value.toPrecision(6)).toString();
  const numeric = (el, key) => el.dataset[key] === undefined ? NaN : Number(el.dataset[key]);
  const node = (name, attrs = {}, text) => {
    const el = document.createElementNS(NS, name);
    Object.entries(attrs).forEach(([key, value]) => el.setAttribute(key, String(value)));
    if (text !== undefined) el.textContent = text;
    return el;
  };
  const range = (values, zero = false, relativeSpan = 0) => {
    values = values.filter(finite); if (zero) values.push(0);
    if (!values.length) return [0, 1];
    let lo = Math.min(...values), hi = Math.max(...values);
    let span = hi - lo;
    if (!span) span = Math.max(Math.abs(lo) * .2, .1);
    else if (span < Math.max(Math.abs(lo), Math.abs(hi)) * relativeSpan) {
      span = Math.max(Math.abs(lo), Math.abs(hi)) * relativeSpan;
      const center = (lo + hi) / 2;
      lo = center - span / 2; hi = center + span / 2;
    }
    return [lo - span * .09, hi + span * .09];
  };
  function ticks(limits, count) {
    const rough = (limits[1] - limits[0]) / Math.max(1, count - 1);
    const power = 10 ** Math.floor(Math.log10(rough));
    const fraction = rough / power;
    const step = (fraction > 7 ? 10 : fraction > 3 ? 5 : fraction > 1.5 ? 2 : 1) * power;
    const output = [];
    for (let v = Math.ceil(limits[0] / step) * step; v <= limits[1] + step * .00001; v += step) {
      output.push(Math.abs(v) < step * 1e-9 ? 0 : Number(v.toPrecision(10)));
      if (output.length > 20) break;
    }
    return output.length < 2 && count < 20 ? ticks(limits, count + 1) : output;
  }
  function logTicks(limits, count) {
    const values = [];
    for (let exponent = Math.floor(limits[0]); exponent <= Math.ceil(limits[1]); exponent++) {
      [1, 2, 5].forEach(multiplier => {
        const value = multiplier * 10 ** exponent, position = Math.log10(value);
        if (position >= limits[0] && position <= limits[1]) values.push(value);
      });
    }
    if (values.length < 2) return ticks(limits.map(value => 10 ** value), count);
    if (values.length <= count) return values;
    return Array.from({length: count}, (_, index) => values[Math.round(index * (values.length - 1) / (count - 1))]);
  }
  const canvas = document.createElement('canvas');
  const context = canvas.getContext('2d');
  context.font = '13px system-ui';
  function wrap(text, width) {
    const words = String(text).split(/\s/).filter(Boolean);
    const output = []; let line = '';
    words.forEach(word => {
      if (line && context.measureText(`${line} ${word}`).width > width) { output.push(line); line = ''; }
      if (context.measureText(word).width > width) {
        for (const letter of word) {
          if (context.measureText(line + letter).width > width && line) { output.push(line); line = ''; }
          line += letter;
        }
      } else line += (line ? ' ' : '') + word;
    });
    if (line) output.push(line);
    return output.length ? output : [''];
  }
  function text(svg, value, x, y, {anchor = 'start', width = Infinity, className = '', lineHeight = 16} = {}) {
    const lines = finite(width) ? wrap(value, width) : [value];
    const el = node('text', {x, y, 'text-anchor': anchor, class: className});
    lines.forEach((line, i) => el.append(node('tspan', {x, dy: i ? lineHeight : 0}, line)));
    svg.append(el); return {el, height: lines.length * lineHeight};
  }
  const tooltip = document.createElement('div');
  tooltip.className = 'axr-chart-tooltip'; tooltip.id = 'axr-chart-tooltip';
  tooltip.setAttribute('role', 'tooltip'); tooltip.hidden = true; document.body.append(tooltip);
  function hideTooltip() { tooltip.hidden = true; }
  function inspectPoint(group, point, figure) {
    figure.querySelectorAll('[data-label-model]').forEach(el =>
      el.classList.toggle('axr-highlight', el.dataset.labelModel === group.dataset.modelId));
    tooltip.textContent = point.detail; tooltip.hidden = false;
    const box = group.getBoundingClientRect(), tip = tooltip.getBoundingClientRect();
    tooltip.style.left = `${Math.max(12, Math.min(box.x + 10, innerWidth - tip.width - 12))}px`;
    tooltip.style.top = `${Math.max(12, box.top > tip.height + 20 ? box.top - tip.height - 10 :
      Math.min(box.bottom + 10, innerHeight - tip.height - 12))}px`;
    const detail = figure.querySelector('.axr-chart-detail');
    if (detail) { detail.textContent = point.detail; figure.dataset.inspected = 'true'; }
  }
  function glyph(svg, point, x, y, figure, index, points) {
    const group = node('g', {class: 'axr-focus-point', tabindex: index === 0 ? 0 : -1,
      role: 'img', 'aria-label': point.detail, 'aria-describedby': tooltip.id,
      'data-chart-point': '', 'data-model-id': point.model, 'data-value-x': point.x, 'data-value-y': point.y});
    // A transparent hit target makes a small statistical point usable by touch.
    group.append(node('circle', {cx: x, cy: y, r: 12, fill: 'transparent'}));
    group.append(node('circle', {cx: x, cy: y, r: point.frontier ? 5.5 : point.radius || 4.2, fill: point.color,
      class: 'axr-point', ...(point.frontier ? {style: 'stroke:#203b2b;stroke-width:2.5'} : {})}));
    group.addEventListener('pointerenter', () => inspectPoint(group, point, figure));
    group.addEventListener('pointerleave', () => { if (document.activeElement !== group) hideTooltip(); });
    group.addEventListener('focus', () => inspectPoint(group, point, figure));
    group.addEventListener('blur', hideTooltip);
    group.addEventListener('click', () => { group.focus(); inspectPoint(group, point, figure); });
    group.addEventListener('keydown', event => {
      if (event.key === 'Escape') { hideTooltip(); return; }
      const groups = Array.from(svg.querySelectorAll('[data-chart-point]'));
      let next;
      if (['ArrowRight', 'ArrowDown'].includes(event.key)) next = (index + 1) % points.length;
      if (['ArrowLeft', 'ArrowUp'].includes(event.key)) next = (index + points.length - 1) % points.length;
      if (event.key === 'Home') next = 0;
      if (event.key === 'End') next = points.length - 1;
      if (next !== undefined) {
        event.preventDefault(); groups.forEach((item, i) => { item.tabIndex = i === next ? 0 : -1; });
        groups[next]?.focus({preventScroll: true});
      }
    });
    svg.append(group); return group;
  }
  function directLabels(svg, points, px, py, box) {
    const placed = [];
    const ordered = points.map(point => ({point, x: px(point.x), y: py(point.y)})).sort((a, b) => a.y - b.y);
    context.save(); context.font = '600 12px system-ui';
    if (box.labelLeft !== undefined) {
      const labels = ordered.map(item => ({...item, height: wrap(item.point.label, box.labelWidth).length * 15}));
      let floor = box.top;
      labels.forEach(item => { item.top = Math.max(floor, item.y - item.height / 2); floor = item.top + item.height + 10; });
      let ceiling = box.bottom;
      [...labels].reverse().forEach(item => {
        item.top = Math.min(item.top, ceiling - item.height); ceiling = item.top - 10;
      });
      labels.forEach(({point, x, y, top, height}) => {
        svg.append(node('line', {x1: x, y1: y, x2: box.labelLeft - 5, y2: top + height / 2,
          class: 'axr-leader', stroke: point.color, 'data-label-model': point.model}));
        const label = text(svg, point.label, box.labelLeft, top + 12,
          {width: box.labelWidth, className: 'axr-model-label', lineHeight: 15}).el;
        label.dataset.labelModel = point.model;
      });
      context.restore(); return true;
    }
    const overlap = (a, b, pad = 5) => a.left < b.right + pad && a.right > b.left - pad &&
      a.top < b.bottom + pad && a.bottom > b.top - pad;
    let fits = true;
    ordered.forEach(({point, x, y}) => {
      const maxWidth = Math.min(170, box.right - box.left - 16);
      const lines = wrap(point.label, maxWidth), height = lines.length * 15;
      const measured = Math.min(maxWidth, Math.max(...lines.map(line => context.measureText(line).width)));
      const preferred = x > (box.left + box.right) / 2 ? 'end' : 'start';
      let chosen;
      for (const shift of [-10, 20, -28, 38, -46, 56, -64, 74, -82, 92]) {
        for (const anchor of [preferred, preferred === 'end' ? 'start' : 'end']) {
          const tx = anchor === 'end' ? Math.max(box.left + measured, x - 9) : Math.min(box.right - measured, x + 9);
          const ty = Math.max(box.top + 14, Math.min(box.bottom - height + 10, y + shift));
          const rect = {left: anchor === 'end' ? tx - measured : tx, right: anchor === 'end' ? tx : tx + measured,
            top: ty - 12, bottom: ty + height - 12};
          const collisions = placed.filter(other => overlap(rect, other)).length + ordered.filter(other =>
            other.point !== point && overlap(rect, {left: other.x - 4, right: other.x + 4,
              top: other.y - 4, bottom: other.y + 4}, 1)).length;
          const score = collisions * 1000 + Math.abs(ty - y) + Math.abs(tx - x) * .05;
          if (!chosen || score < chosen.score) chosen = {tx, ty, anchor, rect, score};
          if (collisions === 0) break;
        }
        if (chosen.score < 1000) break;
      }
      placed.push(chosen.rect);
      if (chosen.score >= 1000) fits = false;
      svg.append(node('line', {x1: x, y1: y, x2: chosen.tx, y2: chosen.ty - 4,
        class: 'axr-leader', stroke: point.color, 'data-label-model': point.model}));
      const label = text(svg, point.label, chosen.tx, chosen.ty,
        {anchor: chosen.anchor, width: maxWidth, className: 'axr-model-label', lineHeight: 15}).el;
      label.dataset.labelModel = point.model;
    });
    context.restore(); return fits;
  }
  function modelKey(figure, points) {
    figure.querySelector('.axr-chart-key')?.remove();
    const models = [...new Map(points.map(point => [point.model, point])).values()];
    if (models.length < 2 || figure.dataset.kind === 'cost') return;
    const key = document.createElement('ul'); key.className = 'axr-chart-key';
    models.forEach(point => {
      const item = document.createElement('li'), mark = document.createElement('i');
      mark.style.background = point.color; mark.setAttribute('aria-hidden', 'true');
      item.append(mark, document.createTextNode(point.label)); key.append(item);
    });
    figure.querySelector('.axr-chart-viewport').before(key);
  }
  let printing = false;
  function render(figure, reserveLabels = false) {
    const viewport = figure.querySelector('.axr-chart-viewport');
    const measuredWidth = Math.floor(viewport.getBoundingClientRect().width);
    const width = printing ? 320 : measuredWidth;
    if (measuredWidth < 150) return;
    const original = retained.get(figure), state = window.AutoXplainRReport?.getState() || {};
    const primary = figure.dataset.primaryModel;
    const points = original.filter(point => !primary || point.model === primary || point.model === state.comparisonModelId);
    const maximumCount = Math.max(1, ...points.map(point => finite(point.count) ? point.count : 0));
    points.forEach(point => { point.radius = finite(point.count) ? 10 * Math.sqrt(point.count / maximumCount) : 4.2; });
    if (!points.length) return;
    const categorical = figure.dataset.kind === 'category', cost = figure.dataset.kind === 'cost';
    const logCost = cost && state.costScale === 'log' && points.every(point => finite(point.x) && point.x > 0);
    const xLabel = figure.dataset.xLabel + (logCost ? ' · log scale' : '');
    const zero = figure.dataset.zero === 'true';
    const categories = [...new Set(points.map(point => point.category))];
    const models = [...new Set(points.map(point => point.model))];
    const support = points.filter(point => !primary || point.model === primary);
    const hasCounts = support.some(point => finite(point.n));
    const hasSupport = ['effect', 'category'].includes(figure.dataset.kind) && support.some(point => finite(point.n) || finite(point.support));
    let xlimits = range(categorical ? points.flatMap(point => [point.y, point.low, point.high]) :
      points.flatMap(point => [point.x, point.left, point.right]), categorical && zero, cost ? .001 : 0);
    let ylimits = range(points.flatMap(point => [point.y, point.low, point.high]), zero, cost ? .001 : 0);
    if (figure.dataset.reference === 'identity') xlimits = ylimits = range(points.flatMap(point => [point.x, point.y]));
    if (cost && points.every(point => point.x >= 0)) xlimits[0] = Math.max(0, xlimits[0]);
    if (cost && points.every(point => point.y >= 0)) ylimits[0] = Math.max(0, ylimits[0]);
    if (figure.dataset.kind === 'histogram' && points.every(point => point.y >= 0)) ylimits[0] = 0;
    if (finite(numeric(figure, 'xMin')) && finite(numeric(figure, 'xMax'))) xlimits = [numeric(figure, 'xMin'), numeric(figure, 'xMax')];
    if (finite(numeric(figure, 'yMin')) && finite(numeric(figure, 'yMax'))) ylimits = [numeric(figure, 'yMin'), numeric(figure, 'yMax')];
    if (logCost) {
      const minimum = Math.min(...points.map(point => point.x));
      xlimits = [Math.log10(Math.max(xlimits[0], minimum / 1.1)), Math.log10(xlimits[1])];
    }
    const labelLines = wrap(categorical ? figure.dataset.xLabel : figure.dataset.yLabel, width - 20);
    const top = 20 + labelLines.length * 16;
    const left = categorical ? Math.min(155, Math.max(105, width * .36)) :
      Math.max(width < 420 ? 48 : 58, ...ticks(ylimits, 5).map(value => context.measureText(number(value)).width + 12));
    const labelColumn = cost && (reserveLabels === true || points.length > 5);
    const labelWidth = labelColumn ? Math.min(180, Math.max(116, width * .36)) : 0;
    const right = width - (labelColumn ? labelWidth + 24 : categorical && hasSupport ? 55 : 20);
    const rowHeight = categorical ? Math.max(42, models.length * 18 + 20,
      ...categories.map(category => wrap(category, left - 18).length * 16 + 12)) : 0;
    context.save(); context.font = '600 12px system-ui';
    const labelHeight = labelColumn ? points.reduce((total, point) => total + wrap(point.label, labelWidth).length * 15 + 10, 0) - 10 : 0;
    context.restore();
    const plotHeight = categorical ? Math.max(100, categories.length * rowHeight) :
      cost ? Math.max(190, labelHeight, Math.min(360, points.length * 38)) : width < 420 ? 180 : 210;
    const bottom = top + plotHeight;
    const bottomLabel = wrap(categorical ? figure.dataset.yLabel : xLabel, right - left);
    const supportHeight = hasSupport && !categorical ? 80 : 0;
    const height = bottom + 42 + bottomLabel.length * 16 + supportHeight;
    const svg = node('svg', {viewBox: `0 0 ${width} ${height}`, role: 'group',
      'aria-label': `${figure.dataset.yLabel} by ${xLabel}. Arrow keys move between values. Values and support follow the chart.`,
      'data-chart-layout': 'responsive', 'data-axis-type': categorical ? 'categorical' : 'numeric',
      'data-x-scale': logCost ? 'log' : 'linear',
      ...(cost ? {'data-label-layout': labelColumn ? 'column' : 'nearby'} : {})});
    text(svg, categorical ? figure.dataset.xLabel : figure.dataset.yLabel, 2, 15, {width: width - 4});
    const px = value => left + ((logCost ? Math.log10(value) : value) - xlimits[0]) /
      (xlimits[1] - xlimits[0]) * (right - left);
    const py = value => bottom - (value - ylimits[0]) / (ylimits[1] - ylimits[0]) * plotHeight;
    const xTickCount = Math.max(labelColumn ? 2 : 3, Math.floor((right - left) / 70));
    (logCost ? logTicks(xlimits, xTickCount) : ticks(xlimits, xTickCount)).forEach(value => {
      const x = px(value);
      if (categorical) svg.append(node('line', {x1: x, x2: x, y1: top, y2: bottom, class: 'axr-grid'}));
      text(svg, number(value), x, bottom + 20, {anchor: 'middle'});
    });
    if (!categorical) ticks(ylimits, 5).forEach(value => {
      svg.append(node('line', {x1: left, x2: right, y1: py(value), y2: py(value), class: 'axr-grid'}));
      text(svg, number(value), left - 8, py(value) + 4, {anchor: 'end'});
    });
    svg.append(node('line', {x1: left, x2: right, y1: bottom, y2: bottom, class: 'axr-axis'}));
    if (zero) svg.append(node('line', categorical ? {x1: px(0), x2: px(0), y1: top, y2: bottom, class: 'axr-zero'} :
      {x1: left, x2: right, y1: py(0), y2: py(0), class: 'axr-zero'}));
    if (figure.dataset.reference === 'identity') svg.append(node('line', {x1: px(xlimits[0]), x2: px(xlimits[1]),
      y1: py(xlimits[0]), y2: py(xlimits[1]), class: 'axr-zero'}));
    text(svg, categorical ? figure.dataset.yLabel : xLabel, (left + right) / 2, bottom + 40,
      {anchor: 'middle', width: right - left});
    if (cost) {
      const frontier = [...new Map(points.filter(point => point.frontier && finite(point.x) && finite(point.y))
        .map(point => [`${point.x},${point.y}`, point])).values()].sort((a, b) => a.x - b.x);
      if (frontier.length > 1) {
        // A better score becomes available only at the next measured cost.
        // Equal measurements share a vertex; every model keeps its own glyph.
        const steps = [`${px(frontier[0].x)},${py(frontier[0].y)}`];
        frontier.slice(1).forEach((point, index) => {
          steps.push(`${px(point.x)},${py(frontier[index].y)}`, `${px(point.x)},${py(point.y)}`);
        });
        svg.append(node('polyline', {class: 'axr-frontier', fill: 'none', stroke: '#203b2b',
          'stroke-width': 2, 'stroke-dasharray': '6 4', 'stroke-linejoin': 'round',
          'aria-hidden': 'true', 'pointer-events': 'none', points: steps.join(' ')}));
      }
    }
    if (figure.dataset.kind === 'effect') models.forEach(model => {
      const group = points.filter(point => point.model === model).sort((a, b) => a.x - b.x);
      if (group.every(point => finite(point.low) && finite(point.high))) {
        svg.append(node('polygon', {class: 'axr-band', fill: group[0].color, points:
          [...group.map(point => `${px(point.x)},${py(point.low)}`),
            ...[...group].reverse().map(point => `${px(point.x)},${py(point.high)}`)].join(' ')}));
      }
      svg.append(node('polyline', {class: 'axr-line', stroke: group[0].color,
        points: group.map(point => `${px(point.x)},${py(point.y)}`).join(' ')}));
    });
    if (categorical) categories.forEach((category, index) => {
      const y = top + index * rowHeight + rowHeight / 2;
      const lines = wrap(category, left - 18);
      text(svg, category, left - 10, y + 4 - (lines.length - 1) * 8,
        {anchor: 'end', width: left - 18});
      const reference = support.find(point => point.category === category);
      if (hasSupport && reference) text(svg, finite(reference.n) ? number(reference.n) :
        finite(reference.support) ? number(reference.support) : '—', width - 3, y + 4, {anchor: 'end'});
    });
    if (categorical && hasSupport) text(svg, hasCounts ? 'Rows' : 'Support', width - 3, top - 9, {anchor: 'end'});
    points.forEach((point, index) => {
      const cx = px(categorical ? point.y : point.x);
      const cy = categorical ? top + categories.indexOf(point.category) * rowHeight + rowHeight / 2 +
        (models.indexOf(point.model) - (models.length - 1) / 2) * 15 : py(point.y);
      if (categorical && finite(point.low) && finite(point.high)) {
        svg.append(node('line', {x1: px(point.low), x2: px(point.high), y1: cy, y2: cy,
          stroke: point.color, 'stroke-width': 2}));
      }
      if (figure.dataset.kind === 'histogram' && finite(point.left) && finite(point.right)) {
        svg.append(node('rect', {x: px(point.left), y: py(point.y), width: Math.max(1, px(point.right) - px(point.left) - 1),
          height: Math.abs(py(0) - py(point.y)), fill: point.color, opacity: .6}));
      }
      glyph(svg, point, cx, cy, figure, index, points);
    });
    if (cost && !directLabels(svg, points, px, py, {top, bottom, left, right,
      ...(labelColumn ? {labelLeft: right + 18, labelWidth} : {})})) return render(figure, true);
    if (hasSupport && !categorical) {
      const supportTop = bottom + 55 + bottomLabel.length * 16;
      const values = support.map(point => hasCounts ? point.n : point.support).filter(finite);
      const maximum = Math.max(...values, 1);
      text(svg, hasCounts ? 'Rows in each interval' : 'Relative support (maximum = 1)', left, supportTop, {width: right - left});
      const floor = supportTop + 35;
      svg.append(node('line', {x1: left, x2: right, y1: floor, y2: floor, class: 'axr-axis'}));
      text(svg, number(maximum), left - 8, supportTop + 14, {anchor: 'end'});
      text(svg, '0', left - 8, floor + 4, {anchor: 'end'});
      support.forEach(point => {
        const value = hasCounts ? point.n : point.support;
        if (!finite(value)) return;
        const x = finite(point.binLeft) ? px(point.binLeft) : px(point.x) - 2;
        const w = finite(point.binLeft) ? Math.max(0, px(point.x) - x) : 4;
        svg.append(node('rect', {x, y: floor - value / maximum * 22, width: w, height: value / maximum * 22,
          class: 'axr-support'}));
      });
    }
    modelKey(figure, points); viewport.replaceChildren(svg);
    figure.querySelector('.axr-comparison-status')?.remove();
    if (primary && state.comparisonModelId && !points.some(point => point.model === state.comparisonModelId)) {
      const status = document.createElement('p'); status.className = 'axr-comparison-status axr-chart-note';
      status.textContent = 'The selected comparison has no retained curve for this input and class. Only the primary curve is shown.';
      viewport.before(status);
    }
    const detail = figure.querySelector('.axr-chart-detail');
    if (detail) detail.textContent = '';
    delete figure.dataset.inspected;
    figure.dataset.visibleModels = models.join(',');
  }
  figures.forEach(figure => {
    retained.set(figure, Array.from(figure.querySelectorAll('[data-chart-source]')).map(el => ({
      x: numeric(el, 'x'), y: numeric(el, 'y'), low: numeric(el, 'low'), high: numeric(el, 'high'),
      n: numeric(el, 'n'), count: numeric(el, 'count'), left: numeric(el, 'left'), right: numeric(el, 'right'), support: numeric(el, 'support'), binLeft: numeric(el, 'binLeft'),
      model: el.dataset.model, label: el.dataset.label, category: el.dataset.category,
      detail: el.dataset.detail, color: el.dataset.color, frontier: el.dataset.frontier === 'true'
    })));
    render(figure);
  });
  let pending;
  const refresh = () => {
    cancelAnimationFrame(pending); pending = requestAnimationFrame(() => { hideTooltip(); figures.forEach(render); });
  };
  const observer = new ResizeObserver(refresh);
  figures.forEach(figure => observer.observe(figure.querySelector('.axr-chart-viewport')));
  ['page-change', 'model-change', 'feature-change', 'comparison-change', 'class-change', 'chart-change'].forEach(name =>
    document.addEventListener(`axr:${name}`, refresh));
  addEventListener('beforeprint', () => { printing = true; hideTooltip(); figures.forEach(render); });
  addEventListener('afterprint', () => { printing = false; refresh(); });
  addEventListener('scroll', () => {
    const point = document.activeElement;
    if (!tooltip.hidden && point?.matches('[data-chart-point]')) {
      inspectPoint(point, {detail: point.getAttribute('aria-label')}, point.closest('.axr-chart'));
    } else hideTooltip();
  }, {passive: true});
  window.AutoXplainRCharts = {refresh};
})();
