(() => {
  'use strict';
  const all = (selector, root = document) => Array.from(root.querySelectorAll(selector));
  const pages = all('.workspace-page');
  const modelControls = all('.model-select');
  const metricControl = document.querySelector('#metric-select');
  const resourceControl = document.querySelector('#resource-select');
  const featureChoices = new Map();
  let model = modelControls[0]?.value;
  const higher = new Set(['accuracy', 'auc', 'roc_auc', 'balanced_accuracy', 'r_squared', 'macro_recall']);
  function chooseFeature(panel, requested) {
    const control = panel.querySelector('.feature-select');
    if (!control) return;
    const available = Array.from(control.options, option => option.value);
    const feature = available.includes(requested) ? requested : available[0];
    control.value = feature || '';
    featureChoices.set(panel.dataset.modelPanel, feature);
    all('[data-feature-panel]', panel).forEach(el => { el.hidden = el.dataset.featurePanel !== feature; });
    all('[data-pick-feature]', panel).forEach(el => {
      const selected = el.dataset.pickFeature === feature;
      el.classList.toggle('is-selected', selected);
      el.setAttribute('aria-pressed', String(selected));
    });
  }
  function chooseModel(id) {
    if (!modelControls.some(control => Array.from(control.options).some(option => option.value === id))) return;
    model = id;
    modelControls.forEach(control => { control.value = id; });
    all('[data-model-panel]').forEach(panel => {
      panel.hidden = panel.dataset.modelPanel !== id;
      chooseFeature(panel, featureChoices.get(panel.dataset.modelPanel));
    });
  }
  function showPage(hash, focus = false) {
    let target;
    try { target = document.getElementById(decodeURIComponent(hash.replace(/^#/, ''))); } catch (_) { return; }
    const page = target?.closest('.workspace-page') || pages[0];
    pages.forEach(el => { el.hidden = el !== page; });
    all('[data-page-link]').forEach(link => {
      const selected = link.dataset.pageLink === page.id;
      link.setAttribute('aria-selected', String(selected));
      link.setAttribute('tabindex', selected ? '0' : '-1');
      if (selected) link.setAttribute('aria-current', 'page');
      else link.removeAttribute('aria-current');
    });
    if (target) {
      for (let parent = target.parentElement; parent; parent = parent.parentElement) {
        if (parent.tagName === 'DETAILS') parent.open = true;
      }
    }
    if (focus) {
      const heading = page.querySelector('h2');
      heading.setAttribute('tabindex', '-1');
      heading.focus({preventScroll: true});
      window.scrollTo({top: 0, left: 0, behavior: 'instant'});
    }
  }
  function chooseMetrics() {
    const metric = metricControl?.value;
    const resource = resourceControl?.value;
    all('[data-score-column]').forEach(el => { el.hidden = el.dataset.scoreColumn !== metric; });
    all('[data-metric-definition]').forEach(el => { el.hidden = el.dataset.metricDefinition !== metric; });
    all('[data-cost-plot]').forEach(el => {
      el.hidden = el.dataset.costPlot !== metric || el.dataset.resource !== resource;
    });
    const rows = all('[data-model-row]');
    rows.sort((a, b) => {
      const left = a.getAttribute(`data-value-${metric}`), right = b.getAttribute(`data-value-${metric}`);
      if (left === '') return 1;
      if (right === '') return -1;
      return (Number(left) - Number(right)) * (higher.has(metric) ? -1 : 1);
    });
    const summary = document.querySelector('#score-summary');
    const first = rows[0];
    if (summary && first) {
      const value = first.getAttribute(`data-value-${metric}`);
      summary.textContent = value === '' ? 'This score is unavailable for the retained models.' :
        `${first.querySelector('.model-link').textContent.trim()} has the ${higher.has(metric) ? 'highest' : 'lowest'} ` +
        `${metricControl.selectedOptions[0].textContent} on these ${summary.dataset.rows} ${summary.dataset.role} rows: ` +
        `${Number(value).toPrecision(4)}.`;
    }
    sortTable(metric, higher.has(metric) ? -1 : 1);
  }
  function sortTable(key, direction) {
    const table = document.querySelector('.model-table');
    if (!table) return;
    const rows = all('tbody tr', table);
    rows.sort((a, b) => {
      const left = a.getAttribute(`data-value-${key}`), right = b.getAttribute(`data-value-${key}`);
      if (left === '') return 1;
      if (right === '') return -1;
      return (Number(left) - Number(right)) * direction;
    });
    rows.forEach(row => table.tBodies[0].appendChild(row));
    all('thead th', table).forEach(th => th.removeAttribute('aria-sort'));
    const button = all('[data-sort]', table).find(el => el.dataset.sort === key);
    if (button) button.parentElement.setAttribute('aria-sort', direction === 1 ? 'ascending' : 'descending');
  }
  modelControls.forEach(control => control.addEventListener('change', () => chooseModel(control.value)));
  function navigate(hash) {
    if (location.hash !== hash) history.pushState(null, '', hash);
    showPage(hash, true);
  }
  all('[data-pick-model]').forEach(link => link.addEventListener('click', event => {
    event.preventDefault();
    chooseModel(link.dataset.pickModel);
    // hashchange does not fire when the same destination is selected again.
    navigate('#patterns');
  }));
  all('.feature-select').forEach(control => control.addEventListener('change', () =>
    chooseFeature(control.closest('[data-model-panel]'), control.value)));
  all('[data-pick-feature]').forEach(button => button.addEventListener('click', () =>
    chooseFeature(button.closest('[data-model-panel]'), button.dataset.pickFeature)));
  all('[data-page-link], .wordmark').forEach(link => link.addEventListener('click', event => {
    event.preventDefault();
    navigate(link.hash);
  }));
  all('[data-sort]').forEach(button => button.addEventListener('click', () => {
    const ascending = button.parentElement.getAttribute('aria-sort') !== 'ascending';
    sortTable(button.dataset.sort, ascending ? 1 : -1);
  }));
  all('[data-pair-detail]').forEach(button => button.addEventListener('click', () => {
    all('[data-pair-detail]').forEach(el => el.classList.toggle('is-selected', el === button));
    document.querySelector('#pair-detail').textContent = button.dataset.pairDetail;
  }));
  metricControl?.addEventListener('change', chooseMetrics);
  resourceControl?.addEventListener('change', chooseMetrics);
  const printControls = all('select').map(control => {
    const label = document.createElement('span');
    label.className = 'print-selection';
    control.after(label);
    return {control, label};
  });
  addEventListener('beforeprint', () => {
    printControls.forEach(({control, label}) => { label.textContent = control.selectedOptions[0]?.textContent || ''; });
    document.activeElement?.blur();
    window.scrollTo({top: 0, left: 0, behavior: 'instant'});
  });
  document.querySelector('#print-report')?.addEventListener('click', () => window.print());
  addEventListener('hashchange', () => showPage(location.hash, true));
  all('.help').forEach(help => {
    const button = help.querySelector('.help-button');
    const tooltip = help.querySelector('.help-tip');
    function position() {
      const box = button.getBoundingClientRect();
      const width = Math.min(340, innerWidth - 24);
      tooltip.style.width = `${width}px`;
      tooltip.style.left = `${Math.max(12, Math.min(box.left, innerWidth - width - 12))}px`;
      tooltip.style.top = `${Math.min(box.bottom + 8, innerHeight - tooltip.offsetHeight - 12)}px`;
    }
    help.addEventListener('pointerenter', position);
    button.addEventListener('focus', position);
    button.addEventListener('click', () => {
      const opened = button.getAttribute('aria-expanded') !== 'true';
      button.setAttribute('aria-expanded', String(opened));
      help.classList.toggle('is-open', opened);
      help.classList.remove('dismissed');
      position();
    });
    button.addEventListener('keydown', event => {
      if (event.key === 'Escape') {
        button.setAttribute('aria-expanded', 'false');
        help.classList.remove('is-open');
        help.classList.add('dismissed');
      }
    });
    button.addEventListener('blur', () => help.classList.remove('dismissed'));
    help.addEventListener('pointerleave', () => help.classList.remove('dismissed'));
  });
  const navigation = document.querySelector('.explorer-nav');
  navigation.setAttribute('role', 'tablist');
  navigation.setAttribute('aria-orientation', innerWidth > 760 ? 'vertical' : 'horizontal');
  addEventListener('resize', () => navigation.setAttribute('aria-orientation',
    innerWidth > 760 ? 'vertical' : 'horizontal'));
  const tabs = all('[data-page-link]');
  tabs.forEach((tab, index) => {
    tab.id = `tab-${tab.dataset.pageLink}`;
    tab.setAttribute('role', 'tab');
    tab.setAttribute('aria-controls', tab.dataset.pageLink);
    const page = document.getElementById(tab.dataset.pageLink);
    page.setAttribute('role', 'tabpanel');
    page.setAttribute('aria-labelledby', tab.id);
    tab.addEventListener('keydown', event => {
      let next;
      if (['ArrowDown', 'ArrowRight'].includes(event.key)) next = (index + 1) % tabs.length;
      if (['ArrowUp', 'ArrowLeft'].includes(event.key)) next = (index + tabs.length - 1) % tabs.length;
      if (event.key === 'Home') next = 0;
      if (event.key === 'End') next = tabs.length - 1;
      if (next !== undefined) {
        event.preventDefault();
        tabs[next].focus();
        tabs[next].click();
        tabs[next].focus();
      }
    });
  });
  document.body.classList.add('has-js');
  chooseModel(model);
  chooseMetrics();
  showPage(location.hash);
})();
