(() => {
  'use strict';
  const source = document.getElementById('axr-predictions-payload');
  if (!source) return;
  let view;
  try { view = JSON.parse(source.textContent); } catch (_) { return; }
  const format = value => value === null || !Number.isFinite(value) ? 'Not defined' :
    Number.isInteger(value) ? String(value) : Number(value.toPrecision(5)).toString();
  let selectedRowKey = null;
  function updateBinaryCases(panel, model, threshold) {
    const table = panel.querySelector('.prediction-cases');
    if (view.mode !== 'rows' || !table || !Array.isArray(model.cases)) return;
    const negative = model.labels.find(label => label !== model.positive);
    const ordered = model.cases.map((row, index) => {
      const predicted = row.probability >= threshold ? model.positive : negative;
      return {row, index, predicted, wrong: predicted !== row.observed,
        predictedProbability: predicted === model.positive ? row.probability : 1 - row.probability};
    }).sort((a, b) => Number(b.wrong) - Number(a.wrong) ||
      a.row.observed_probability - b.row.observed_probability || a.index - b.index);
    const body = document.createElement('tbody');
    for (const entry of ordered.slice(0, 10)) {
      const row = document.createElement('tr');
      row.dataset.caseRow = entry.row.row_key;
      row.classList.toggle('is-selected', entry.row.row_key === selectedRowKey);
      const sourceCell = document.createElement('td'), link = document.createElement('a');
      link.href = '#data'; link.dataset.navigate = ''; link.dataset.selectRow = entry.row.row_key;
      link.textContent = `${entry.row.source} ${entry.row.source_row}`;
      sourceCell.append(link); row.append(sourceCell);
      for (const value of [entry.row.observed, entry.predicted,
        format(entry.row.observed_probability), format(entry.predictedProbability)]) {
        const cell = document.createElement('td'); cell.textContent = value; row.append(cell);
      }
      body.append(row);
    }
    table.tBodies[0].replaceWith(body);
    table.caption.textContent = `Up to ten exported evaluation records at cutoff ${threshold.toFixed(2)}; ` +
      'mistakes first, then lowest probability assigned to the observed class.';
  }
  for (const model of view.models || []) {
    const panel = Array.from(document.querySelectorAll('[data-prediction-model]'))
      .find(item => item.dataset.predictionModel === model.model_id);
    if (!panel) continue;
    const slider = panel.querySelector('[data-prediction-cutoff]');
    if (slider && Array.isArray(model.cutoffs) && model.cutoffs.length === 101) {
      const update = () => {
        const record = model.cutoffs[Number(slider.value)];
        if (!record) return;
        const threshold = Number(record.threshold.toFixed(2));
        panel.querySelector('[data-cutoff-value]').textContent = record.threshold.toFixed(2);
        panel.querySelector('[data-cutoff-rule]').textContent =
          `Probability at least ${record.threshold.toFixed(2)} predicts ${model.positive}.`;
        for (const field of panel.querySelectorAll('[data-cutoff-metric]')) {
          field.textContent = record.display?.[field.dataset.cutoffMetric] ?? format(record[field.dataset.cutoffMetric]);
        }
        for (const cell of panel.querySelectorAll('[data-confusion-table] td')) {
          const observedPositive = cell.dataset.observed === model.positive;
          const predictedPositive = cell.dataset.predicted === model.positive;
          const key = observedPositive ? (predictedPositive ? 'tp' : 'fn') : (predictedPositive ? 'fp' : 'tn');
          const total = observedPositive ? record.tp + record.fn : record.tn + record.fp;
          cell.querySelector('[data-cell-count]').textContent = String(record[key]);
          cell.querySelector('[data-cell-rate]').textContent = total ? `${(100 * record[key] / total).toFixed(1)}%` : 'Not defined';
        }
        slider.setAttribute('aria-valuetext', `${record.threshold.toFixed(2)}; predicts ${model.positive} at or above this probability`);
        updateBinaryCases(panel, model, threshold);
        const command = panel.querySelector('[data-prediction-code]');
        if (command && model.r_code) {
          command.textContent = model.r_code.prediction + '\n' +
            model.r_code.cutoff_prefix + threshold.toFixed(2) + model.r_code.cutoff_suffix;
        }
      };
      slider.disabled = false;
      slider.addEventListener('input', update);
      update();
    }
  }
  document.addEventListener('click', event => {
    const link = event.target.closest('.prediction-workspace [data-select-row]');
    if (!link) return;
    window.dispatchEvent(new CustomEvent('axr:select-row', {detail: {row_key: link.dataset.selectRow}}));
  });
  window.addEventListener('axr:row-selected', event => {
    selectedRowKey = event.detail?.row_key ?? null;
    for (const row of document.querySelectorAll('.prediction-workspace [data-case-row]')) {
      row.classList.toggle('is-selected', row.dataset.caseRow === selectedRowKey);
    }
  });
})();
