(() => {
  'use strict';
  const source = document.getElementById('axr-predictions-payload');
  if (!source) return;
  let view;
  try { view = JSON.parse(source.textContent); } catch (_) { return; }
  const format = value => value === null || !Number.isFinite(value) ? 'Not defined' :
    Number.isInteger(value) ? String(value) : Number(value.toPrecision(5)).toString();
  let selectedRowKey = null;
  const caseStores = new WeakMap();
  function caseStore(cases) {
    if (Array.isArray(cases)) return {length: cases.length, value: (i, field) => cases[i][field]};
    if (caseStores.has(cases)) return caseStores.get(cases);
    if (cases?.layout !== 'case-columns-v1') return {length: 0};
    const columns = new Map();
    const store = {length: cases.length, value: (i, field) => {
      if (!columns.has(field)) {
        const values = window.AutoXplainRPayload.decodeBlock(cases.columns[field]);
        if (!Array.isArray(values) || values.length !== cases.length) throw new Error('Incomplete prediction cases.');
        columns.set(field, values);
      }
      return columns.get(field)[i];
    }};
    caseStores.set(cases, store);
    return store;
  }
  function updateBinaryCases(panel, model, threshold) {
    const table = panel.querySelector('.prediction-cases');
    if (view.mode !== 'rows' || !table || !model.cases) return;
    const cases = caseStore(model.cases);
    const negative = model.labels.find(label => label !== model.positive);
    const ordered = [];
    const compare = (a, b) => Number(b.wrong) - Number(a.wrong) ||
      a.observedProbability - b.observedProbability || a.index - b.index;
    for (let index = 0; index < cases.length; index++) {
      const probability = cases.value(index, 'probability');
      const predicted = probability >= threshold ? model.positive : negative;
      const entry = {index, predicted, wrong: predicted !== cases.value(index, 'observed'),
        observedProbability: cases.value(index, 'observed_probability'),
        predictedProbability: predicted === model.positive ? probability : 1 - probability};
      if (ordered.length < 10 || compare(entry, ordered[ordered.length - 1]) < 0) {
        ordered.push(entry); ordered.sort(compare);
        if (ordered.length > 10) ordered.pop();
      }
    }
    const body = document.createElement('tbody');
    for (const entry of ordered) {
      const row = document.createElement('tr');
      const key = cases.value(entry.index, 'row_key');
      row.dataset.caseRow = key;
      row.classList.toggle('is-selected', key === selectedRowKey);
      const sourceCell = document.createElement('td'), link = document.createElement('a');
      link.href = '#data'; link.dataset.navigate = ''; link.dataset.selectRow = key;
      link.textContent = `${cases.value(entry.index, 'source')} ${cases.value(entry.index, 'source_row')}`;
      sourceCell.append(link); row.append(sourceCell);
      for (const value of [cases.value(entry.index, 'observed'), entry.predicted,
        format(entry.observedProbability), format(entry.predictedProbability)]) {
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
      const update = (refreshCases = true) => {
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
        if (refreshCases) {
          try { updateBinaryCases(panel, model, threshold); }
          catch (error) {
            const cases = panel.querySelector('.prediction-cases');
            if (cases) cases.hidden = true;
            if (!panel.querySelector('[data-case-decode-error]')) {
              const message = document.createElement('p'); message.dataset.caseDecodeError = '';
              message.setAttribute('role', 'alert');
              message.textContent = 'Individual prediction records could not be opened. The evaluation counts above still use all evaluation rows. ' + error.message;
              panel.querySelector('.prediction-records')?.append(message);
            }
          }
        }
        const command = panel.querySelector('[data-prediction-code]');
        if (command && model.r_code) {
          command.textContent = model.r_code.prediction + '\n' +
            model.r_code.cutoff_prefix + threshold.toFixed(2) + model.r_code.cutoff_suffix;
        }
      };
      slider.disabled = false;
      slider.addEventListener('input', () => update());
      update(false);
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
