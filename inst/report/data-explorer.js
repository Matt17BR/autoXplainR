(() => {
  'use strict';
  const node = document.getElementById('axr-data-payload');
  const root = document.getElementById('data');
  if (!node || !root) return;
  let payload;
  try { payload = JSON.parse(node.textContent); } catch (_) { return; }
  const profile = payload.profile;
  const arr = value => value == null ? [] : Array.isArray(value) ? value : [value];
  const columns = arr(profile.columns);
  const columnsByName = new Map(columns.map(column => [column.name, column]));
  const names = columns.map(column => column.name);
  const records = arr(payload.rows);
  const $ = id => document.getElementById(id);
  const colors = {training: '#17654e', evaluation: '#9a481b'};
  const initialStage = profile.stages.raw ? 'raw' : 'processed';
  const state = {column: profile.target, stage: initialStage, view: 'distribution',
    split: profile.stages[initialStage].training_available ? 'both' : 'evaluation',
    scale: 'percent', y: names.find(name => name !== profile.target) || profile.target,
    filters: [], sort: 'source_row', direction: 1, page: 0, selected: null};
  const finite = value => typeof value === 'number' && Number.isFinite(value);
  const missing = value => value === null || value === undefined;
  const number = value => missing(value) ? 'Unavailable' : finite(value) ?
    value !== 0 && (Math.abs(value) < .001 || Math.abs(value) >= 1e6) ? value.toExponential(2) :
      new Intl.NumberFormat('en', {maximumSignificantDigits: 4}).format(value) : String(value);
  const element = (tag, text, className) => {
    const result = document.createElement(tag);
    if (text != null) result.textContent = text;
    if (className) result.className = className;
    return result;
  };
  const svgElement = (tag, attributes = {}, text) => {
    const result = document.createElementNS('http://www.w3.org/2000/svg', tag);
    Object.entries(attributes).forEach(([key, value]) => result.setAttribute(key, value));
    if (text != null) result.textContent = text;
    return result;
  };
  const svg = (width, height, label) => svgElement('svg', {
    viewBox: `0 0 ${width} ${height}`, role: 'img', 'aria-label': label, class: 'data-plot'
  });
  const availableWidth = host => Math.max(220, Math.floor(host.clientWidth || root.querySelector('.data-main').clientWidth || 760));
  const textMeasure = document.createElement('canvas').getContext('2d');
  textMeasure.font = '14px system-ui';
  function fitLabel(value, pixels) {
    let text = String(value);
    if (textMeasure.measureText(text).width <= pixels) return text;
    while (text.length > 1 && textMeasure.measureText(text + '…').width > pixels) text = text.slice(0, -1);
    return text + '…';
  }
  function niceInterval(span, intervals, minimum = 0) {
    const desired = Math.max(minimum, span / intervals);
    const power = 10 ** Math.floor(Math.log10(desired));
    return Math.max(minimum, [1, 2, 5, 10].find(value => value * power >= desired) * power);
  }
  function table(headers, rows, caption) {
    const result = element('table');
    if (caption) result.append(element('caption', caption));
    const head = element('thead'), heading = element('tr');
    headers.forEach(text => { const cell = element('th', text); cell.scope = 'col'; heading.append(cell); });
    head.append(heading); result.append(head);
    const body = element('tbody');
    rows.forEach(values => {
      const row = element('tr');
      values.forEach(value => row.append(element('td', number(value))));
      body.append(row);
    });
    result.append(body);
    return result;
  }
  const splits = () => (state.split === 'both' ? ['training', 'evaluation'] : [state.split])
    .filter(split => stage()[split + '_available']);
  const stage = () => profile.stages[state.stage];
  const axisFor = name => stage().columns[name]?.axis;
  const rowValue = (row, name, basis = state.stage) => row[basis]?.[name];
  const nonfinite = (row, name, basis = state.stage) => arr(row.nonfinite?.[basis]).includes(name);
  const formatRowValue = (row, name, basis = state.stage) => nonfinite(row, name, basis) ? 'Non-finite' :
    formatValue(rowValue(row, name, basis), profile.stages[basis]?.columns[name]?.axis);
  function formatValue(value, axis) {
    if (missing(value)) return 'Missing';
    if (finite(value) && ['date', 'datetime'].includes(axis?.kind)) {
      const date = new Date(value * (axis.kind === 'date' ? 86400000 : 1000));
      return axis.kind === 'date' ? date.toISOString().slice(0, 10) : date.toISOString().replace('T', ' ').slice(0, 16) + ' UTC';
    }
    return number(value);
  }
  function code(value, axis) {
    if (missing(value) || !axis || axis.status !== 'available') return null;
    if (axis.kind === 'categorical') {
      const levels = arr(axis.levels), index = levels.indexOf(String(value));
      if (index >= 0) return index + 1;
      return arr(axis.known_levels).includes(String(value)) ? axis.other_code : axis.novel_code;
    }
    if (!finite(value)) return null;
    const breaks = arr(axis.breaks);
    if (value < breaks[0]) return 1;
    if (value > breaks[breaks.length - 1]) return breaks.length + 1;
    if (value === breaks[breaks.length - 1]) return breaks.length;
    let low = 0, high = breaks.length - 1;
    while (low + 1 < high) {
      const middle = Math.floor((low + high) / 2);
      if (value >= breaks[middle]) low = middle; else high = middle;
    }
    return low + 2;
  }
  function matches(row, filter) {
    const value = rowValue(row, filter.column, filter.stage);
    if (filter.op === 'nonfinite') return nonfinite(row, filter.column, filter.stage);
    if (filter.op === 'missing') return missing(value) && !nonfinite(row, filter.column, filter.stage);
    if (filter.op === 'present') return !missing(value);
    if (missing(value)) return false;
    if (filter.op === 'in') return arr(filter.values).includes(String(value));
    if (filter.op === 'eq') return String(value) === filter.value;
    if (filter.op === 'contains') return String(value).toLowerCase().includes(filter.value.toLowerCase());
    if (!finite(value) || !Number.isFinite(Number(filter.value))) return false;
    return filter.op === 'ge' ? value >= Number(filter.value) : value <= Number(filter.value);
  }
  function filteredRows() {
    return records.filter(row => (state.split === 'both' || row.partition === state.split) &&
      (state.stage !== 'processed' || row.retained) && state.filters.every(filter => matches(row, filter)));
  }
  function sampleDistribution(name, split) {
    const axis = axisFor(name), rows = filteredRows().filter(row => row.partition === split);
    if (axis.status !== 'available') return {status: axis.status, reason: axis.reason, n_total: rows.length};
    const counts = arr(axis.labels).map(() => 0), values = [], distinct = new Set();
    let missingCount = 0, nonfiniteCount = 0;
    rows.forEach(row => {
      const value = rowValue(row, name), bin = code(value, axis);
      if (nonfinite(row, name)) nonfiniteCount++; else if (missing(value)) missingCount++; else distinct.add(value);
      if (bin != null) counts[bin - 1]++;
      if (finite(value)) values.push(value);
    });
    return {status: axis.status, n_total: rows.length, n_missing: missingCount, n_nonfinite: nonfiniteCount,
      n_used: counts.reduce((a, b) => a + b, 0), n_unique: distinct.size, counts,
      mean: values.length ? values.reduce((a, b) => a + b, 0) / values.length : null};
  }
  function distribution(name, split) {
    return state.filters.length ? sampleDistribution(name, split) : stage().columns[name][split];
  }
  function population() {
    if (state.view === 'relationships' && !state.filters.length && !storedPair() && records.length) {
      $('data-population').textContent = exportedPairScope();
      return;
    }
    const basis = state.filters.length ? 'Filtered exported sample' : 'Full data';
    $('data-population').textContent = `${basis} · ${stage().population}`;
  }
  function renderDistribution() {
    const axis = axisFor(state.column), selected = splits();
    $('data-variable-title').textContent = state.column;
    const column = columns.find(item => item.name === state.column);
    $('data-variable-role').textContent = `${column.role} · ${axis.kind}`;
    const summary = $('data-summary'); summary.replaceChildren();
    selected.forEach(split => {
      const data = distribution(state.column, split);
      const item = element('div', null, 'data-stat');
      item.append(element('strong', `${number(data.n_total)} ${split} rows`));
      const details = [];
      if (data.n_missing != null) details.push(`${number(data.n_missing)} missing`);
      if (data.n_unique != null) details.push(`${number(data.n_unique)} distinct`);
      item.append(element('span', details.join(' · ') || data.reason || 'Column unavailable'));
      if (data.n_nonfinite > 0) item.append(element('span', `${number(data.n_nonfinite)} non-finite`));
      if (finite(data.mean)) item.append(element('span', `Mean ${formatValue(data.mean, axis)}`));
      if (data.quantiles?.median != null) item.append(element('span', `Median ${formatValue(data.quantiles.median, axis)}`));
      summary.append(item);
    });
    const host = $('data-distribution'); host.replaceChildren();
    if (axis.status !== 'available') {
      host.append(element('p', axis.reason || 'This column has no usable display values.'));
      $('data-distribution-table').replaceChildren();
      $('data-distribution-note').textContent = 'Missing or unsupported values are not replaced with zero.';
      return;
    }
    const labels = arr(axis.labels), numeric = axis.kind !== 'categorical';
    const series = selected.map(split => {
      const data = distribution(state.column, split);
      return {split, data, values: arr(data.counts).map(count => state.scale === 'count' ? count :
        data.n_total ? 100 * count / data.n_total : 0)};
    });
    const active = labels.map((label, i) => i).filter(i => numeric || series.some(item => item.values[i] > 0));
    if (!active.length) { host.append(element('p', 'No non-missing values in this selection.')); return; }
    const maximum = Math.max(1, ...series.flatMap(item => item.values));
    const width = availableWidth(host), height = numeric ? (axis.kind === 'numeric' ? 316 : 300) : active.length * 36 + 38;
    const plot = svg(width, height, `Distribution of ${state.column}`);
    if (numeric) {
      const left = 55, bottom = 244, usable = width - left - 15, step = usable / labels.length;
      const yStep = niceInterval(maximum, 4, state.scale === 'count' ? 1 : .1);
      const yMaximum = Math.ceil(maximum / yStep) * yStep;
      for (let tick = 0; tick <= Math.round(yMaximum / yStep); tick++) {
        const value = tick * yStep, y = bottom - value / yMaximum * 200;
        plot.append(svgElement('line', {x1: left, x2: width - 15, y1: y, y2: y, class: 'data-grid'}),
          svgElement('text', {x: left - 9, y: y + 5, 'text-anchor': 'end', 'data-axis-tick': 'y'},
            new Intl.NumberFormat('en', {maximumFractionDigits: state.scale === 'count' ? 0 : 1}).format(value)));
      }
      series.forEach((item, s) => item.values.forEach((value, i) => {
        const bar = svgElement('rect', {x: left + i * step + s * step / series.length + 1,
          y: bottom - value / yMaximum * 200, width: Math.max(1, step / series.length - 2),
          height: value / yMaximum * 200, fill: colors[item.split]});
        bar.append(svgElement('title', {}, `${labels[i]} · ${item.split}: ${arr(item.data.counts)[i]} rows`));
        plot.append(bar);
      }));
      const breaks = arr(axis.breaks);
      if (axis.kind === 'numeric') {
        const low = breaks[0], high = breaks[breaks.length - 1];
        const xStep = niceInterval(high - low, width < 440 ? 2 : 4);
        const first = Math.ceil(low / xStep), last = Math.floor(high / xStep);
        for (let index = first; index <= last; index++) {
          const value = index * xStep;
          const x = left + step + (value - low) / (high - low) * (usable - 2 * step);
          plot.append(svgElement('line', {x1: x, x2: x, y1: bottom, y2: bottom + 6, class: 'data-grid'}),
            svgElement('text', {x, y: 268, 'text-anchor': x < left + 25 ? 'start' : x > width - 40 ? 'end' : 'middle',
              'data-axis-tick': 'x'}, number(value)));
        }
        [0, breaks.length - 1].forEach((index, end) => {
          const label = svgElement('text', {x: end ? width - 15 : left, y: 285,
            'text-anchor': end ? 'end' : 'start'}, (end ? '>' : '<') + formatValue(breaks[index], axis));
          label.append(svgElement('title', {}, `${end ? 'Above' : 'Below'} displayed-range bin boundary: ${breaks[index]}`));
          plot.append(label);
        });
      } else {
        const ticks = width < 440 ? 2 : 4;
        const tickBins = Array.from({length: ticks}, (_, i) => Math.round((labels.length - 1) * i / (ticks - 1)));
        tickBins.forEach(i => {
          const value = i === 0 ? '<' + formatValue(breaks[0], axis) : i === labels.length - 1 ?
            '>' + formatValue(breaks[breaks.length - 1], axis) : formatValue((breaks[i - 1] + breaks[i]) / 2, axis);
          plot.append(svgElement('text', {x: left + (i + .5) * step, y: 268,
            'text-anchor': i === 0 ? 'start' : i === labels.length - 1 ? 'end' : 'middle'}, value));
        });
      }
      plot.append(svgElement('text', {x: left, y: 18}, state.scale === 'count' ? 'Number of rows' : 'Percent of all rows'),
        svgElement('text', {x: left + usable / 2, y: height - 5, 'text-anchor': 'middle'}, fitLabel(state.column, usable)));
    } else {
      const left = Math.min(210, Math.floor(width * .36)), usable = width - left - 72;
      active.forEach((i, r) => {
        const y = 16 + r * 36;
        const text = fitLabel(labels[i], left - 10);
        const label = svgElement('text', {x: left, y: y + 13, 'text-anchor': 'end'}, text);
        label.append(svgElement('title', {}, labels[i])); plot.append(label);
        series.forEach((item, s) => {
          const value = item.values[i], bar = svgElement('rect', {x: left + 8, y: y + s * 12,
            width: usable * value / maximum, height: 10, fill: colors[item.split]});
          bar.append(svgElement('title', {}, `${labels[i]} · ${item.split}: ${arr(item.data.counts)[i]} rows`));
          plot.append(bar, svgElement('text', {x: width - 55, y: y + s * 12 + 10},
            state.scale === 'count' ? number(value) : value.toFixed(1) + '%'));
        });
      });
    }
    host.append(plot);
    const legend = element('p', null, 'data-legend');
    selected.forEach(split => { const key = element('span', split); key.style.color = colors[split]; legend.append(key); });
    host.append(legend);
    $('data-distribution-note').textContent = `${axis.basis}. Percentages use all rows in each split. ` +
      (numeric ? 'Missing and non-finite values are counted separately; numeric bins include below/above-range bins.' :
        'Missing values are counted separately; zero-count categories remain in the exact counts table.');
    const rows = labels.map((label, i) => [label, ...selected.map(split => arr(distribution(state.column, split).counts)[i] ?? null)]);
    rows.push(['Missing', ...selected.map(split => distribution(state.column, split).n_missing)]);
    rows.push(['Non-finite', ...selected.map(split => distribution(state.column, split).n_nonfinite)]);
    $('data-distribution-table').replaceChildren(table(['Value/bin', ...selected], rows, 'Exact counts in displayed populations'));
  }
  function sampledPair(xName, yName, split) {
    const xAxis = axisFor(xName), yAxis = axisFor(yName), rows = filteredRows().filter(row => row.partition === split);
    const counts = new Map(); let complete = 0;
    rows.forEach(row => {
      const x = code(rowValue(row, xName), xAxis), y = code(rowValue(row, yName), yAxis);
      if (x == null || y == null) return;
      complete++; const key = `${x}:${y}`;
      counts.set(key, (counts.get(key) || 0) + 1);
    });
    const eventBins = new Map();
    if (yName === profile.target && profile.task === 'binary' && profile.positive != null) {
      rows.forEach(row => {
        const x = code(rowValue(row, xName), xAxis), y = rowValue(row, yName);
        if (x == null || missing(y)) return;
        const bin = eventBins.get(x) || {x, n: 0, events: 0};
        bin.n++; if (String(y) === String(profile.positive)) bin.events++;
        eventBins.set(x, bin);
      });
    }
    return {status: 'available', n_total: rows.length, n_complete: complete, n_excluded: rows.length - complete,
      cells: Array.from(counts, ([key, n]) => ({x: Number(key.split(':')[0]), y: Number(key.split(':')[1]), n})),
      conditional_event: Array.from(eventBins.values(), bin => ({...bin, rate: bin.events / bin.n})),
      association: sampleAssociation(xName, yName, rows)};
  }
  function sampleAssociation(xName, yName, rows) {
    const xAxis = axisFor(xName), yAxis = axisFor(yName);
    if (xAxis?.status !== 'available' || yAxis?.status !== 'available') {
      return {status: 'unavailable', n: 0, reason: 'A selected column has no usable display values.'};
    }
    const numericX = xAxis.kind !== 'categorical', numericY = yAxis.kind !== 'categorical';
    const data = rows.map(row => [rowValue(row, xName), rowValue(row, yName)])
      .filter(pair => !missing(pair[0]) && !missing(pair[1]) &&
        (!numericX || finite(pair[0])) && (!numericY || finite(pair[1])));
    const n = data.length, x = data.map(pair => pair[0]), y = data.map(pair => pair[1]);
    if (n < 3 || new Set(x).size < 2 || new Set(y).size < 2) return {status: 'unavailable', n,
      reason: 'At least three complete pairs and variation in both columns are needed.'};
    if (numericX && numericY) {
      const ranks = values => {
        const order = values.map((value, index) => ({value, index})).sort((a, b) => a.value - b.value), ranked = [];
        for (let i = 0; i < order.length;) {
          let end = i + 1; while (end < order.length && order[end].value === order[i].value) end++;
          for (let j = i; j < end; j++) ranked[order[j].index] = (i + 1 + end) / 2;
          i = end;
        }
        return ranked;
      };
      const a = ranks(x), b = ranks(y), mean = (n + 1) / 2;
      const covariance = a.reduce((sum, value, i) => sum + (value - mean) * (b[i] - mean), 0);
      const variance = values => values.reduce((sum, value) => sum + (value - mean) ** 2, 0);
      return {status: 'available', method: 'Spearman correlation (signed)', n,
        value: covariance / Math.sqrt(variance(a) * variance(b))};
    }
    if (numericX !== numericY) {
      const values = numericX ? x : y, categories = numericX ? y : x;
      const mean = values.reduce((a, b) => a + b, 0) / n, groups = new Map();
      values.forEach((value, i) => { const g = groups.get(categories[i]) || {n: 0, sum: 0}; g.n++; g.sum += value; groups.set(categories[i], g); });
      const between = Array.from(groups.values()).reduce((sum, group) => sum + group.n * (group.sum / group.n - mean) ** 2, 0);
      const total = values.reduce((sum, value) => sum + (value - mean) ** 2, 0);
      return {status: 'available', method: 'Correlation ratio (unsigned)', n, value: Math.sqrt(between / total)};
    }
    const a = new Map(), b = new Map(), cells = new Map();
    data.forEach(([x, y]) => {
      a.set(x, (a.get(x) || 0) + 1); b.set(y, (b.get(y) || 0) + 1);
      const key = JSON.stringify([x, y]), cell = cells.get(key) || {x, y, n: 0}; cell.n++; cells.set(key, cell);
    });
    const chi = Array.from(cells.values()).reduce((sum, cell) => sum + cell.n ** 2 / (a.get(cell.x) * b.get(cell.y) / n), 0) - n;
    return {status: 'available', method: "Cramer's V (unsigned)", n,
      value: Math.sqrt(Math.max(0, chi) / (n * Math.min(a.size - 1, b.size - 1)))};
  }
  function storedPair() {
    const indices = [names.indexOf(state.column) + 1, names.indexOf(state.y) + 1].sort((a, b) => a - b);
    return stage().pairs[indices.join('_')];
  }
  function exportedPairScope() {
    const rows = filteredRows();
    const counts = splits().map(split => `${split}: ${number(rows.filter(row => row.partition === split).length)} of ` +
      `${number(stage()[split + '_rows'])} rows`);
    return `${state.filters.length ? 'Filtered exported sample' : 'Exported records'} · ` +
      `${state.stage === 'raw' ? 'Raw supplied values' : 'Values used by models'} · ${counts.join('; ')}`;
  }
  function pairData() {
    const stored = storedPair();
    if (state.filters.length || (!stored && records.length)) {
      const x = state.column === profile.target ? state.y : state.column;
      const y = state.column === profile.target ? state.column : state.y;
      return {x, y, source: 'exported', ...Object.fromEntries(splits().map(split => [split, sampledPair(x, y, split)]))};
    }
    return stored;
  }
  function visibleBins(axis, cells, dimension) {
    const all = arr(axis.labels).map((_, index) => index + 1);
    const occupied = new Set(cells.map(cell => cell[dimension]));
    return all.filter(bin => axis.kind === 'categorical' ? occupied.has(bin) :
      (bin > 1 && bin < all.length) || occupied.has(bin));
  }
  function binTick(axis, bin) {
    const labels = arr(axis.labels), index = bin - 1;
    if (axis.kind === 'categorical') return labels[index].length > 12 ? labels[index].slice(0, 11) + '…' : labels[index];
    const breaks = arr(axis.breaks);
    return index === 0 ? '<' + formatValue(breaks[0], axis) : index === labels.length - 1 ?
      '>' + formatValue(breaks[breaks.length - 1], axis) : formatValue((breaks[index - 1] + breaks[index]) / 2, axis);
  }
  function renderEventRate(pair) {
    if (pair.y !== profile.target || profile.task !== 'binary' || profile.positive == null) return false;
    const series = splits().map(split => ({split, bins: arr(pair[split]?.conditional_event)}));
    const all = series.flatMap(item => item.bins);
    if (!all.length) return false;
    const axis = axisFor(pair.x), labels = arr(axis.labels);
    const active = visibleBins(axis, all, 'x');
    const host = $('data-conditional'), plotWidth = availableWidth(host);
    const plot = svg(plotWidth, 290, `Observed rate of ${profile.target}=${profile.positive} by ${pair.x}`);
    const left = 55, width = plotWidth - left - 15, bottom = 212, height = 175, step = width / Math.max(1, active.length);
    for (let i = 0; i <= 4; i++) {
      const y = bottom - i / 4 * height;
      plot.append(svgElement('line', {x1: left, x2: plotWidth - 15, y1: y, y2: y, class: 'data-grid'}),
        svgElement('text', {x: left - 8, y: y + 5, 'text-anchor': 'end'}, `${i * 25}%`));
    }
    series.forEach((item, s) => item.bins.forEach(bin => {
      const index = active.indexOf(bin.x);
      const point = svgElement('circle', {cx: left + (index + .5) * step + (s - (series.length - 1) / 2) * 5,
        cy: bottom - bin.rate * height, r: 4, fill: colors[item.split]});
      point.append(svgElement('title', {}, `${item.split} · ${labels[bin.x - 1]}: ${bin.events}/${bin.n} = ${number(100 * bin.rate)}%`));
      plot.append(point);
    }));
    const ticks = plotWidth < 440 ? (['date', 'datetime'].includes(axis.kind) ? 2 : 3) : 5;
    const stride = Math.max(1, Math.ceil((active.length - 1) / (ticks - 1)));
    active.forEach((bin, index) => {
      if (index % stride && index !== active.length - 1) return;
      const label = svgElement('text', {x: left + (index + .5) * step, y: 238,
        'text-anchor': index === 0 ? 'start' : index === active.length - 1 ? 'end' : 'middle'},
      fitLabel(binTick(axis, bin), width / (ticks - 1) - 8));
      label.append(svgElement('title', {}, labels[bin - 1])); plot.append(label);
    });
    plot.append(svgElement('text', {x: left, y: 19}, fitLabel(`Observed ${profile.target} = ${profile.positive}`, width)),
      svgElement('text', {x: left + width / 2, y: 276, 'text-anchor': 'middle'}, fitLabel(pair.x, width)));
    host.append(element('h4', `Observed event rate · ${profile.target} = ${profile.positive}`), plot);
    const legend = element('p', null, 'data-legend');
    series.forEach(item => { const key = element('span', item.split); key.style.color = colors[item.split]; legend.append(key); });
    host.append(legend, element('p', 'Each point is events / rows with a usable pair in that bin. Empty bins have no point. These exploratory observed rates are not model probabilities or causal effects.', 'data-chart-note'));
    const details = element('details'); details.append(element('summary', 'Event counts and rates by bin'));
    details.append(table(['Split', pair.x, 'Rows', 'Events', 'Event rate (%)'], series.flatMap(item =>
      item.bins.map(bin => [item.split, labels[bin.x - 1], bin.n, bin.events, bin.rate * 100])),
    `Event: ${profile.target} = ${profile.positive}; denominator excludes missing pairs`));
    host.append(details); return true;
  }
  function renderPair() {
    const host = $('data-pair'); host.replaceChildren();
    $('data-association').replaceChildren();
    $('data-pair-table').replaceChildren(); $('data-conditional').replaceChildren();
    if (state.column === state.y) {
      host.append(element('p', 'Choose a different second column to inspect a relationship.'));
      $('data-pair-note').textContent = ''; return;
    }
    const pair = pairData();
    if (!pair) {
      host.append(element('p', 'This pair was outside the aggregate computation budget. Its individual distributions remain available.'));
      $('data-pair-note').textContent = `${profile.pair_coverage.included} of ${profile.pair_coverage.total} pairs included. ` +
        'Row mode permits selected-pair exploration of the exported sample.'; return;
    }
    const xAxis = axisFor(state.column), yAxis = axisFor(state.y);
    const association = $('data-association');
    if (pair.source === 'exported') {
      association.append(element('p', exportedPairScope()));
    }
    splits().forEach(split => {
      const reading = pair[split]?.association;
      association.append(element('p', reading?.status === 'available' ?
        `${split}: ${reading.method} ${number(reading.value)} · n = ${number(reading.n)}` :
        `${split}: association unavailable · n = ${number(reading?.n)}. ${reading?.reason || 'No retained unbinned pair values.'}`));
    });
    association.append(element('span', 'Computed from unbinned complete pairs. Small association does not establish independence or exclude nonlinear or joint dependence.'));
    if (xAxis.status !== 'available' || yAxis.status !== 'available') {
      host.append(element('p', 'A selected column has no usable display values.')); $('data-pair-note').textContent = ''; return;
    }
    const labelsX = arr(xAxis.labels), labelsY = arr(yAxis.labels), swap = pair.x !== state.column;
    const cellsBySplit = splits().map(split => ({split, data: pair[split], cells: arr(pair[split]?.cells).map(cell =>
      swap ? {x: cell.y, y: cell.x, n: cell.n} : cell)}));
    const allCells = cellsBySplit.flatMap(item => item.cells);
    const activeX = visibleBins(xAxis, allCells, 'x'), activeY = visibleBins(yAxis, allCells, 'y');
    const maxCount = Math.max(1, ...cellsBySplit.flatMap(item => item.cells.map(cell => cell.n)));
    const combined = element('div', null, 'data-density-grid');
    const tableRows = [], notes = [];
    cellsBySplit.forEach(item => {
      const card = element('div', null, 'data-density-card'); card.append(element('h4', item.split));
      if (item.data?.status !== 'available') { card.append(element('p', item.data?.reason || 'Pair unavailable.')); combined.append(card); return; }
      notes.push(`${item.split}: ${number(item.data.n_complete)} complete, ${number(item.data.n_excluded)} excluded`);
      const panelWidth = availableWidth(host) >= 740 ? Math.floor((availableWidth(host) - 14) / 2) : availableWidth(host);
      const left = Math.min(100, Math.floor(panelWidth * .30)), top = 32;
      const dense = xAxis.kind === 'categorical' && activeX.length > 5;
      const width = dense ? Math.max(panelWidth - left - 20, activeX.length * 54) : panelWidth - left - 20;
      const height = yAxis.kind === 'categorical' ? Math.max(56, activeY.length * 30) : 185;
      const plotWidth = left + width + 20, plotHeight = top + height + (xAxis.kind === 'categorical' ? 90 : 75);
      const plot = svg(plotWidth, plotHeight, `${state.column} and ${state.y}, ${item.split}, binned counts`);
      if (dense) {
        card.classList.add('data-plot-scroll'); card.tabIndex = 0;
        card.setAttribute('role', 'region'); card.setAttribute('aria-label', `${activeX.length}-category matrix; scroll horizontally`);
        card.append(element('p', `${activeX.length} categories · scroll to inspect every labeled column`, 'data-chart-note'));
        plot.style.minWidth = `${plotWidth}px`;
      }
      const cellWidth = width / Math.max(1, activeX.length), cellHeight = height / Math.max(1, activeY.length);
      item.cells.forEach(cell => {
        const rectangle = svgElement('rect', {x: left + activeX.indexOf(cell.x) * cellWidth,
          y: top + height - (activeY.indexOf(cell.y) + 1) * cellHeight, width: cellWidth, height: cellHeight,
          fill: colors[item.split], opacity: .18 + .82 * Math.sqrt(cell.n / maxCount), stroke: '#fff', 'stroke-width': .4});
        rectangle.append(svgElement('title', {}, `${labelsX[cell.x - 1]} / ${labelsY[cell.y - 1]}: ${cell.n} rows`));
        plot.append(rectangle); tableRows.push([item.split, labelsX[cell.x - 1], labelsY[cell.y - 1], cell.n]);
      });
      [activeX, activeY].forEach((active, axisIndex) => {
        const axis = axisIndex === 0 ? xAxis : yAxis;
        const labels = arr(axis.labels);
        const tickCount = panelWidth < 400 || ['date', 'datetime'].includes(axis.kind) ? 2 : 3;
        const stride = axis.kind === 'categorical' ? 1 : Math.max(1, Math.ceil((active.length - 1) / (tickCount - 1)));
        active.forEach((bin, index) => {
          if (index % stride && index !== active.length - 1) return;
          const short = fitLabel(binTick(axis, bin), axisIndex === 0 ? Math.max(40, width / tickCount) : left - 10);
          const label = labels[bin - 1];
          const text = axisIndex === 0 ? svgElement('text', {x: left + (index + .5) * cellWidth,
            y: top + height + 24, 'text-anchor': axis.kind === 'categorical' ? 'end' :
              index === 0 ? 'start' : index === active.length - 1 ? 'end' : 'middle'}, short) : svgElement('text', {x: left - 8,
            y: top + height - (index + .5) * cellHeight + 5, 'text-anchor': 'end'}, short);
          if (axisIndex === 0 && axis.kind === 'categorical') {
            text.setAttribute('transform', `rotate(-35 ${left + (index + .5) * cellWidth} ${top + height + 24})`);
          }
          text.append(svgElement('title', {}, label)); plot.append(text);
        });
      });
      plot.append(svgElement('text', {x: left, y: 19}, fitLabel(state.y, width)),
        svgElement('text', {x: left + width / 2, y: plotHeight - 8, 'text-anchor': 'middle'}, fitLabel(state.column, width)));
      card.append(plot); combined.append(card);
    });
    if (renderEventRate(pair)) {
      const details = element('details'); details.append(element('summary', 'Joint counts for the same relationship'), combined);
      host.append(details);
    } else host.append(combined);
    $('data-pair-note').textContent = notes.join(' · ') + `. Darker cells contain more rows; shared count scale up to ${maxCount}. ` +
      'Empty cells mean zero complete pairs. Unused overflow categories are omitted from the plotted axes; exact counts retain their original bin definitions.';
    $('data-pair-table').append(table(['Split', state.column, state.y, 'Rows'], tableRows, 'Occupied cells only; missing pairs excluded'));
    const conditionalRows = [];
    splits().forEach(split => arr(pair[split]?.conditional).forEach(row => {
      conditionalRows.push([split, arr(axisFor(pair.x).labels)[row.x - 1], row.n, row.mean, row.median]);
    }));
    if (conditionalRows.length && pair.y === profile.target) {
      const details = element('details'); details.append(element('summary', `Observed ${profile.target} by ${pair.x}`));
      details.append(table(['Split', pair.x, 'Rows', `Mean ${profile.target}`, `Median ${profile.target}`],
        conditionalRows, 'Observed averages within bins, not model predictions'));
      $('data-conditional').append(details);
    }
  }
  function renderFilterChips() {
    const host = $('data-filter-chips'); host.replaceChildren();
    state.filters.forEach((filter, index) => {
      const chip = element('button', `${filter.stage}: ${filter.column} ${filter.op} ${filter.display ?? filter.value} ×`, 'data-filter-chip');
      chip.type = 'button'; chip.setAttribute('aria-label', `Remove filter ${filter.column} ${filter.op} ${filter.value}`);
      chip.addEventListener('click', () => { state.filters.splice(index, 1); state.page = 0; render(); });
      host.append(chip);
    });
  }
  function selectRow(key, announce = true) {
    const row = records.find(item => item.row_key === key);
    if (!row) return;
    state.selected = key;
    const host = $('data-selected-row'); host.replaceChildren(element('strong',
      `${row.partition} · ${row.source} row ${row.source_row} · ${row.retained ? 'retained' : 'removed by preprocessing'}`));
    host.append(table(['Column', 'Raw supplied value', 'Processed model value'], names.map(name => [name,
      payload.manifest.raw_status === 'available' ? formatRowValue(row, name, 'raw') : 'Unavailable',
      !['target', 'predictor'].includes(columns.find(column => column.name === name).role) ? 'Not used by model' :
        row.retained ? formatRowValue(row, name, 'processed') : 'Row not retained'
    ]), 'The same original observation before and after preprocessing'));
    root.querySelectorAll('[data-row-key]').forEach(node => {
      node.classList.toggle('is-selected', node.dataset.rowKey === key);
    });
    if (announce) window.dispatchEvent(new CustomEvent('axr:row-selected', {detail: {
      row_key: key, partition: row.partition, processed_position: row.processed_position
    }}));
  }
  function renderScatter(rows) {
    const host = $('data-row-scatter'); host.replaceChildren();
    const xAxis = axisFor(state.column), yAxis = axisFor(state.y);
    if (state.column === state.y || !['numeric', 'date', 'datetime'].includes(xAxis.kind) ||
        !['numeric', 'date', 'datetime'].includes(yAxis.kind)) {
      host.append(element('p', 'Individual scatter points are available for two numeric or time columns. Use the joint counts for categories.'));
      return;
    }
    const complete = rows.filter(row => finite(rowValue(row, state.column)) && finite(rowValue(row, state.y)));
    if (!complete.length) { host.append(element('p', 'No complete numeric pairs in the exported selection.')); return; }
    const extent = name => {
      const values = complete.map(row => rowValue(row, name));
      let low = Math.min(...values), high = Math.max(...values);
      if (low === high) { low -= .5; high += .5; }
      return [low, high];
    };
    const plotWidth = availableWidth(host), left = 60, usable = plotWidth - left - 15;
    const x = extent(state.column), y = extent(state.y), plot = svg(plotWidth, 340, 'Linked scatter of exported records');
    plot.setAttribute('role', 'group');
    const px = value => left + (value - x[0]) / (x[1] - x[0]) * usable;
    const py = value => 268 - (value - y[0]) / (y[1] - y[0]) * 225;
    const ticks = plotWidth < 440 ? 2 : 4;
    for (let i = 0; i <= ticks; i++) {
      const a = x[0] + (x[1] - x[0]) * i / ticks, b = y[0] + (y[1] - y[0]) * i / ticks;
      plot.append(svgElement('line', {x1: left, x2: plotWidth - 15, y1: py(b), y2: py(b), class: 'data-grid'}),
        svgElement('text', {x: px(a), y: 296, 'text-anchor': i === 0 ? 'start' : i === ticks ? 'end' : 'middle'},
          fitLabel(formatValue(a, xAxis), usable / ticks - 8)),
        svgElement('text', {x: left - 8, y: py(b) + 5, 'text-anchor': 'end'}, fitLabel(formatValue(b, yAxis), left - 12)));
    }
    complete.forEach((row, index) => {
      const label = `${row.partition} source row ${row.source_row}: ${state.column} ${formatValue(rowValue(row, state.column), xAxis)}, ` +
        `${state.y} ${formatValue(rowValue(row, state.y), yAxis)}`;
      const point = svgElement('circle', {cx: px(rowValue(row, state.column)), cy: py(rowValue(row, state.y)),
        r: 4, fill: colors[row.partition], opacity: .65, tabindex: index === 0 ? '0' : '-1', role: 'button', 'aria-label': label,
        'data-row-key': row.row_key});
      point.append(svgElement('title', {}, label));
      point.addEventListener('click', () => selectRow(row.row_key));
      point.addEventListener('keydown', event => {
        if (event.key === 'Enter' || event.key === ' ') { event.preventDefault(); selectRow(row.row_key); }
        if (['ArrowRight', 'ArrowLeft'].includes(event.key)) {
          event.preventDefault();
          const points = Array.from(plot.querySelectorAll('circle[data-row-key]'));
          const next = (index + (event.key === 'ArrowRight' ? 1 : points.length - 1)) % points.length;
          points.forEach((item, i) => item.setAttribute('tabindex', i === next ? '0' : '-1'));
          points[next].focus();
        }
      });
      plot.append(point);
    });
    plot.append(svgElement('text', {x: left, y: 20}, fitLabel(state.y, usable)),
      svgElement('text', {x: left + usable / 2, y: 328, 'text-anchor': 'middle'}, fitLabel(state.column, usable)));
    host.append(element('h4', `Individual records · ${complete.length} complete exported pairs`),
      element('p', 'Focus a point, then use Left/Right to move and Enter to inspect its record.', 'data-chart-note'), plot);
  }
  function renderRows() {
    if (payload.mode !== 'rows') return;
    const rows = filteredRows();
    $('data-sample-note').textContent = `${records.length} of ${payload.manifest.full_rows} original rows embedded. ` +
      `${payload.manifest.sampling}; seed ${payload.manifest.seed}. Full-data aggregates above remain separate until a filter is applied.`;
    $('data-filter-status').textContent = `${rows.length} matching exported rows · ${state.filters.length} active filters. ` +
      (state.filters.length ? 'Charts now describe the filtered exported sample.' : 'Charts above describe full data; scatter and table describe exported records.');
    renderFilterChips(); renderScatter(rows);
    rows.sort((a, b) => {
      const left = state.sort === 'source_row' ? a.source_row : rowValue(a, state.sort);
      const right = state.sort === 'source_row' ? b.source_row : rowValue(b, state.sort);
      if (missing(left)) return missing(right) ? 0 : 1;
      if (missing(right)) return -1;
      return state.direction * (finite(left) && finite(right) ? left - right : String(left).localeCompare(String(right)));
    });
    const size = 10, lastPage = Math.max(0, Math.ceil(rows.length / size) - 1);
    state.page = Math.min(state.page, lastPage);
    const selected = rows.slice(state.page * size, (state.page + 1) * size);
    const visible = Array.from(new Set([state.column, state.y, profile.target]));
    const output = table(['Record', 'Split', 'Retained', ...visible], [], 'Exported source positions, not the entire original dataset');
    selected.forEach(row => {
      const tr = element('tr'); tr.dataset.rowKey = row.row_key;
      const cell = element('td'), button = element('button', `${row.source}:${row.source_row}`, 'data-row-link');
      button.type = 'button'; button.addEventListener('click', () => selectRow(row.row_key)); cell.append(button);
      tr.append(cell, element('td', row.partition), element('td', row.retained ? 'Yes' : 'No'));
      visible.forEach(name => tr.append(element('td', formatRowValue(row, name))));
      output.tBodies[0].append(tr);
    });
    $('data-row-table').replaceChildren(output);
    $('data-row-page').textContent = rows.length ? `${state.page * size + 1}–${Math.min(rows.length, (state.page + 1) * size)} of ${rows.length}` : 'No matching rows';
    $('data-row-prev').disabled = state.page === 0; $('data-row-next').disabled = state.page === lastPage;
    if (state.selected) selectRow(state.selected, false);
  }
  function render() {
    $('data-column-select').value = state.column;
    root.querySelectorAll('[data-column-name]').forEach(button => {
      const name = button.dataset.columnName, column = columnsByName.get(name);
      const count = splits().reduce((total, split) => total + (distribution(name, split).n_missing || 0), 0);
      button.setAttribute('aria-pressed', String(name === state.column));
      button.querySelector('span').textContent = `${column.role} · ${axisFor(name).kind}` +
        (count > 0 ? ` · ${number(count)} missing` : '');
      const scope = `${state.filters.length ? 'Filtered exported sample' : 'Full available rows'} · ` +
        `${stage().population} · ${splits().join(' and ')}. Missing counts follow the selected values and splits.`;
      button.title = scope;
      button.setAttribute('aria-description', scope);
    });
    root.querySelectorAll('[data-data-panel]').forEach(panel => { panel.hidden = panel.dataset.dataPanel !== state.view; });
    root.querySelectorAll('[data-data-view]').forEach(button => {
      button.setAttribute('aria-pressed', String(button.dataset.dataView === state.view));
    });
    $('data-pair-control').hidden = state.view === 'distribution';
    $('data-scale-control').hidden = state.view !== 'distribution';
    population(); renderDistribution(); renderPair(); renderRows();
  }
  function chooseView(view) {
    if (!['distribution', 'relationships', 'records'].includes(view)) return;
    state.view = view; render();
  }
  function chooseColumn(name) {
    if (!names.includes(name)) return;
    if (state.column === profile.target && name !== profile.target) state.y = profile.target;
    state.column = name; state.page = 0;
    if (state.y === name) state.y = names.find(value => value !== name) || name;
    $('data-y').value = state.y; render();
  }
  root.querySelectorAll('[data-data-view]').forEach(button => button.addEventListener('click', () => chooseView(button.dataset.dataView)));
  root.querySelectorAll('[data-column-name]').forEach(button => button.addEventListener('click', () => chooseColumn(button.dataset.columnName)));
  $('data-column-select').addEventListener('change', event => chooseColumn(event.target.value));
  $('data-search').addEventListener('input', event => {
    const query = event.target.value.toLowerCase();
    root.querySelectorAll('[data-column-name]').forEach(button => { button.hidden = !button.dataset.columnName.toLowerCase().includes(query); });
  });
  [['data-stage', 'stage'], ['data-split', 'split'], ['data-scale', 'scale'], ['data-y', 'y']].forEach(([id, key]) => {
    $(id).addEventListener('change', event => { state[key] = event.target.value; state.page = 0; render(); });
  });
  if (payload.mode === 'rows') {
    const updateFilterInput = () => {
      const axis = axisFor($('data-filter-column').value), input = $('data-filter-value');
      const operator = $('data-filter-op');
      Array.from(operator.options).forEach(option => {
        option.disabled = ['ge', 'le', 'nonfinite'].includes(option.value) && axis.kind === 'categorical' ||
          option.value === 'in' && axis.kind !== 'categorical';
      });
      if (operator.selectedOptions[0].disabled) operator.value = 'eq';
      const op = operator.value;
      input.disabled = ['missing', 'present', 'nonfinite'].includes(op);
      input.parentElement.hidden = op === 'in';
      $('data-filter-level-label').hidden = op !== 'in';
      if (op === 'in') {
        const values = Array.from(new Set(records.map(row => rowValue(row, $('data-filter-column').value))
          .filter(value => !missing(value)).map(String))).sort();
        $('data-filter-levels').replaceChildren(...values.map(value => {
          const option = element('option', value); option.value = value; return option;
        }));
      }
      input.type = axis.kind === 'date' ? 'date' : axis.kind === 'datetime' ? 'datetime-local' :
        ['ge', 'le'].includes(op) ? 'number' : 'text';
      if (input.type === 'number') input.step = 'any';
    };
    $('data-filter-op').addEventListener('change', updateFilterInput);
    $('data-filter-column').addEventListener('change', updateFilterInput);
    $('data-stage').addEventListener('change', updateFilterInput);
    $('data-filter-form').addEventListener('submit', event => {
      event.preventDefault(); const op = $('data-filter-op').value, display = $('data-filter-value').value;
      const column = $('data-filter-column').value, kind = axisFor(column).kind;
      if (axisFor(column).status !== 'available') {
        $('data-filter-status').textContent = 'This column has no values in the selected stage. Choose its raw values when available.';
        return;
      }
      if (op === 'in') {
        const values = Array.from($('data-filter-levels').selectedOptions, option => option.value);
        if (!values.length) { $('data-filter-status').textContent = 'Select at least one category.'; return; }
        state.filters.push({column, op, values, value: '', display: values.join(', '), stage: state.stage});
        state.page = 0; render(); return;
      }
      let value = display;
      if (['date', 'datetime'].includes(kind) && !['missing', 'present', 'nonfinite'].includes(op)) {
        const parsed = Date.parse(kind === 'date' ? display + 'T00:00:00Z' : display + 'Z');
        if (!Number.isFinite(parsed)) { $('data-filter-status').textContent = 'Enter a valid date or UTC date-time.'; return; }
        value = String(parsed / (kind === 'date' ? 86400000 : 1000));
      }
      if (['ge', 'le'].includes(op) && (!value.trim() || !Number.isFinite(Number(value)))) {
        $('data-filter-status').textContent = 'Enter a numeric bound.';
        return;
      }
      state.filters.push({column, op, value, display, stage: state.stage});
      state.page = 0; render();
    });
    $('data-filter-reset').addEventListener('click', () => { state.filters = []; state.page = 0; render(); });
    $('data-row-sort').addEventListener('change', event => { state.sort = event.target.value; state.page = 0; renderRows(); });
    $('data-row-direction').addEventListener('click', event => {
      state.direction *= -1; event.target.textContent = state.direction === 1 ? 'Ascending' : 'Descending'; renderRows();
    });
    $('data-row-prev').addEventListener('click', () => { state.page--; renderRows(); });
    $('data-row-next').addEventListener('click', () => { state.page++; renderRows(); });
    window.addEventListener('axr:select-row', event => {
      const row = records.find(item => item.row_key === event.detail?.row_key);
      if (!row) return;
      state.view = 'records'; state.split = row.partition; state.filters = []; state.sort = 'source_row'; state.direction = 1;
      $('data-split').value = state.split; $('data-row-sort').value = 'source_row';
      $('data-row-direction').textContent = 'Ascending';
      const order = filteredRows().sort((a, b) => a.source_row - b.source_row);
      state.page = Math.max(0, Math.floor(order.findIndex(item => item.row_key === row.row_key) / 10));
      render(); selectRow(row.row_key, true);
      requestAnimationFrame(() => $('data-selected-row').scrollIntoView({block: 'nearest'}));
    });
    updateFilterInput();
  }
  window.AutoXplainRData = {
    selectColumn: chooseColumn,
    selectView: chooseView,
    selectRow: key => payload.mode === 'rows' && window.dispatchEvent(new CustomEvent('axr:select-row', {detail: {row_key: key}})),
    getState: () => ({...state, filters: state.filters.map(filter => ({...filter})), matchingRows: filteredRows().length})
  };
  window.addEventListener('hashchange', () => {
    if (window.location.hash === '#relationships') chooseView('relationships');
  });
  let resizeFrame;
  const redraw = () => {
    cancelAnimationFrame(resizeFrame);
    resizeFrame = requestAnimationFrame(() => {
      if (root.getBoundingClientRect().width > 0) render();
    });
  };
  window.addEventListener('resize', redraw);
  document.addEventListener('axr:page-change', event => {
    if (event.detail?.page === 'data') {
      requestAnimationFrame(() => {
        if (location.hash.split('?')[0] === '#relationships') state.view = 'relationships';
        redraw();
      });
    }
  });
  let observedWidth = 0;
  if (typeof ResizeObserver !== 'undefined') new ResizeObserver(entries => {
    const width = Math.floor(entries[0].contentRect.width);
    if (width > 0 && width !== observedWidth) { observedWidth = width; redraw(); }
  }).observe(root.querySelector('.data-main'));
  if (window.location.hash === '#relationships') state.view = 'relationships';
  root.classList.add('data-enhanced'); render();
})();
