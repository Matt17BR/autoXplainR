(() => {
  'use strict';
  const all = (selector, root = document) => Array.from(root.querySelectorAll(selector));
  const pages = all('.workspace-page');
  const modelControls = all('.model-select');
  const metricControl = document.querySelector('#metric-select');
  const resourceControl = document.querySelector('#resource-select');
  const costScaleControl = document.querySelector('#cost-scale-select');
  const costScaleNote = document.querySelector('#cost-scale-note');
  const scoreScaleControl = document.querySelector('#score-scale-select');
  const scoreScaleNote = document.querySelector('#score-scale-note');
  const classControl = document.querySelector('#effect-class-select');
  const comparisonControl = document.querySelector('#comparison-model-select');
  const comparisonIdentity = document.createElement('div');
  comparisonIdentity.className = 'model-identity comparison-identity';
  comparisonIdentity.hidden = true;
  const higher = new Set(['accuracy', 'auc', 'roc_auc', 'balanced_accuracy', 'r_squared', 'macro_recall']);
  const state = {modelId: modelControls[0]?.value, feature: null, page: pages[0]?.id,
    metric: metricControl?.value, resource: resourceControl?.value, costScale: costScaleControl?.value || 'linear',
    scoreScale: scoreScaleControl?.value || 'linear',
    className: classControl?.value, comparisonModelId: comparisonControl?.value || ''};
  const emit = (name, detail) => document.dispatchEvent(new CustomEvent(`axr:${name}`, {detail, bubbles: true}));
  const options = control => control ? Array.from(control.options, option => option.value) : [];
  function saveState(replace = true) {
    const query = new URLSearchParams();
    for (const key of ['modelId', 'feature', 'metric', 'resource', 'costScale', 'scoreScale', 'className', 'comparisonModelId']) {
      if (state[key] != null && state[key] !== '') query.set(key, state[key]);
    }
    const hash = `#${state.page}${query.size ? `?${query}` : ''}`;
    if (location.hash !== hash) history[replace ? 'replaceState' : 'pushState'](null, '', hash);
  }
  function setFeature(panel, requested) {
    const control = panel.querySelector('.feature-select');
    if (!control) return null;
    const available = options(control);
    const feature = available.includes(requested) ? requested : available[0];
    control.value = feature || '';
    all('[data-feature-panel]', panel).forEach(el => { el.hidden = el.dataset.featurePanel !== feature; });
    all('[data-pick-feature]', panel).forEach(el => {
      const selected = el.dataset.pickFeature === feature;
      el.classList.toggle('is-selected', selected);
      el.setAttribute('aria-pressed', String(selected));
    });
    return feature;
  }
  function selectFeature(feature, {persist = true} = {}) {
    let selected;
    all('[data-model-panel]').filter(panel => panel.dataset.modelPanel === state.modelId).forEach(panel => {
      selected = setFeature(panel, feature) || selected;
    });
    state.feature = selected || feature || null;
    if (persist) saveState();
    emit('feature-change', {...state});
    return state.feature;
  }
  function selectModel(id, {persist = true} = {}) {
    if (!modelControls.some(control => options(control).includes(id))) return false;
    state.modelId = id;
    if (state.comparisonModelId === id) {
      state.comparisonModelId = '';
      if (comparisonControl) comparisonControl.value = '';
    }
    if (comparisonControl) Array.from(comparisonControl.options).forEach(option => {
      option.disabled = option.value === id;
    });
    modelControls.forEach(control => { control.value = id; });
    all('[data-model-panel]').forEach(panel => { panel.hidden = panel.dataset.modelPanel !== id; });
    selectFeature(state.feature, {persist: false});
    all('[data-model-scope]').forEach(el => { el.hidden = el.dataset.modelScope !== id; });
    if (persist) saveState();
    emit('model-change', {...state});
    return true;
  }
  function updateComparisonIdentity() {
    const panels = all('#patterns [data-model-panel]');
    const primary = panels.find(panel => panel.dataset.modelPanel === state.modelId);
    const secondary = panels.find(panel => panel.dataset.modelPanel === state.comparisonModelId);
    const original = secondary?.querySelector('.model-identity:not(.comparison-identity)');
    comparisonIdentity.hidden = !original || !primary || state.modelId === state.comparisonModelId;
    if (comparisonIdentity.hidden) return;
    const heading = document.createElement('strong');
    heading.textContent = `Comparison: ${comparisonControl.selectedOptions[0].textContent}`;
    const settings = original.querySelector('.model-settings').cloneNode(true);
    settings.className = 'comparison-settings';
    comparisonIdentity.replaceChildren(heading, settings, original.querySelector('[data-open-spec]').cloneNode(true));
    comparisonIdentity.dataset.comparisonModel = state.comparisonModelId;
    primary.querySelector('.model-identity:not(.comparison-identity)').after(comparisonIdentity);
  }
  function sortTable(key, direction) {
    const table = document.querySelector('.model-table');
    if (!table) return;
    const rows = all('[data-model-row]', table);
    rows.sort((a, b) => {
      const left = a.getAttribute(`data-value-${key}`), right = b.getAttribute(`data-value-${key}`);
      if (left === '' && right === '') return 0;
      if (left === '') return 1;
      if (right === '') return -1;
      return (Number(left) - Number(right)) * direction;
    });
    rows.forEach(row => table.tBodies[0].appendChild(row));
    all('thead th', table).forEach(th => th.removeAttribute('aria-sort'));
    const button = all('[data-sort]', table).find(el => el.dataset.sort === key);
    if (button) button.parentElement.setAttribute('aria-sort', direction === 1 ? 'ascending' : 'descending');
  }
  function chooseResource({persist = true} = {}) {
    state.resource = resourceControl?.value;
    all('[data-cost-plot]').forEach(el => {
      el.hidden = el.dataset.costPlot !== state.metric || el.dataset.resource !== state.resource;
    });
    if (costScaleControl) {
      const plot = all('[data-cost-plot]').find(el => !el.hidden);
      const costs = plot ? all('[data-chart-source]', plot).map(el => Number(el.dataset.x)) : [];
      const positive = costs.length > 0 && costs.every(value => Number.isFinite(value) && value > 0);
      costScaleControl.disabled = !costs.length;
      costScaleControl.querySelector('option[value="log"]').disabled = !positive;
      if (!positive) costScaleControl.value = 'linear';
      state.costScale = costScaleControl.value;
      if (costScaleNote) {
        costScaleNote.hidden = positive;
        costScaleNote.textContent = positive ? '' : !costs.length ? 'Cost scale is unavailable: no finite comparison.' :
          costs.some(value => value === 0) && /time|prediction/.test(state.resource) ?
            'Log scale needs positive costs. A zero timing may be below timer resolution.' :
          'Log scale needs positive costs; this comparison contains a nonpositive measurement.';
      }
    }
    if (scoreScaleControl) {
      const plot = all('[data-cost-plot]').find(el => !el.hidden);
      const scores = plot ? all('[data-chart-source]', plot).map(el => Number(el.dataset.y)) : [];
      const loss = ['rmse', 'mae', 'mse', 'log_loss', 'brier', 'calibration_error'].includes(state.metric);
      const positive = loss && scores.length > 0 && scores.every(value => Number.isFinite(value) && value > 0);
      scoreScaleControl.disabled = !scores.length;
      scoreScaleControl.querySelector('option[value="log"]').disabled = !positive;
      if (!positive) scoreScaleControl.value = 'linear';
      state.scoreScale = scoreScaleControl.value;
      if (scoreScaleNote) {
        scoreScaleNote.hidden = positive;
        scoreScaleNote.textContent = positive ? '' : !scores.length ? 'Score scale is unavailable: no finite comparison.' :
          !loss ? 'Log score scale is available for positive losses. This metric uses a linear axis.' :
          'Log score scale needs strictly positive losses; this comparison includes a zero or negative score.';
      }
    }
    if (persist) saveState();
    emit('chart-change', {...state});
  }
  function chooseMetric({persist = true, sort = true} = {}) {
    state.metric = metricControl?.value;
    all('[data-score-column]').forEach(el => { el.hidden = el.dataset.scoreColumn !== state.metric; });
    all('[data-metric-definition]').forEach(el => { el.hidden = el.dataset.metricDefinition !== state.metric; });
    chooseResource({persist: false});
    if (sort) sortTable(state.metric, higher.has(state.metric) ? -1 : 1);
    const summary = document.querySelector('#score-summary');
    if (summary && summary.dataset.dynamicSummary !== 'false') {
      summary.textContent = `${summary.dataset.rows} ${summary.dataset.role} rows · ` +
        `${metricControl?.selectedOptions[0]?.textContent || state.metric}: ` +
        `${higher.has(state.metric) ? 'higher' : 'lower'} is better. ` +
        'Table order is descriptive; the primary model is unchanged.';
    }
    if (persist) saveState();
  }
  function chooseClass({persist = true} = {}) {
    state.className = classControl?.value;
    all('[data-class-panel]').forEach(panel => { panel.hidden = panel.dataset.classPanel !== state.className; });
    if (persist) saveState();
    emit('class-change', {...state});
  }
  function parseHash(hash) {
    const [fragment, query = ''] = hash.replace(/^#/, '').split('?');
    let id;
    try { id = decodeURIComponent(fragment); } catch (_) { id = ''; }
    return {id, params: new URLSearchParams(query)};
  }
  function showPage(id, target, focus) {
    const page = document.getElementById(id)?.closest('.workspace-page') || pages[0];
    if (!page) return;
    state.page = page.id;
    pages.forEach(el => { el.hidden = el !== page; });
    all('[data-page-link]').forEach(link => {
      const selected = link.dataset.pageLink === page.id;
      link.setAttribute('aria-selected', String(selected));
      link.tabIndex = selected ? 0 : -1;
      if (selected) link.setAttribute('aria-current', 'page'); else link.removeAttribute('aria-current');
      if (selected && innerWidth <= 760) link.scrollIntoView({block: 'nearest', inline: 'nearest'});
    });
    if (target && target !== page) {
      target.dispatchEvent(new CustomEvent('axr:inspect', {bubbles: true}));
      if (target.tagName === 'DETAILS') target.open = true;
      for (let parent = target.parentElement; parent; parent = parent.parentElement) {
        if (parent.tagName === 'DETAILS') parent.open = true;
      }
    }
    if (focus) {
      const destination = target && target !== page ?
        (target.tagName === 'DETAILS' ? target.querySelector('summary') : target) : page.querySelector('h2');
      if (destination) {
        if (!destination.matches('a,button,input,select,summary,[tabindex]')) destination.tabIndex = -1;
        destination.focus({preventScroll: true});
        if (target && target !== page) destination.scrollIntoView({block: 'center', behavior: 'instant'});
        else window.scrollTo({top: 0, left: 0, behavior: 'instant'});
      }
    }
    emit('page-change', {...state});
  }
  function navigate(hash, {focus = true, persist = true} = {}) {
    const {id, params} = parseHash(hash);
    for (const [key, control] of [['metric', metricControl], ['resource', resourceControl], ['costScale', costScaleControl], ['scoreScale', scoreScaleControl],
      ['className', classControl], ['comparisonModelId', comparisonControl]]) {
      if (params.has(key) && options(control).includes(params.get(key))) control.value = params.get(key);
    }
    if (params.has('feature')) state.feature = params.get('feature');
    if (params.has('modelId')) selectModel(params.get('modelId'), {persist: false});
    else if (params.has('feature')) selectFeature(state.feature, {persist: false});
    chooseMetric({persist: false, sort: false});
    chooseClass({persist: false});
    state.comparisonModelId = comparisonControl?.value || '';
    if (state.comparisonModelId === state.modelId) {
      state.comparisonModelId = '';
      if (comparisonControl) comparisonControl.value = '';
    }
    emit('comparison-change', {...state});
    const target = document.getElementById(id);
    const evidenceScope = target?.dataset.evidenceModel ? target.dataset :
      all('a[data-evidence-model]').find(link => parseHash(link.hash).id === id)?.dataset;
    if (evidenceScope?.evidenceModel) selectModel(evidenceScope.evidenceModel, {persist: false});
    if (evidenceScope?.evidenceFeature) selectFeature(evidenceScope.evidenceFeature, {persist: false});
    showPage(id, target, focus);
    if (persist) {
      if (target && target.id !== state.page) history.pushState(null, '', `#${encodeURIComponent(id)}`);
      else saveState(false);
    }
  }
  modelControls.forEach(control => control.addEventListener('change', () => selectModel(control.value)));
  all('.feature-select').forEach(control => control.addEventListener('change', () => selectFeature(control.value)));
  all('[data-pick-feature]').forEach(button => button.addEventListener('click', () => {
    if (button.dataset.forModel && button.dataset.forModel !== state.modelId) selectModel(button.dataset.forModel);
    selectFeature(button.dataset.pickFeature);
    if (innerWidth <= 940) {
      button.closest("[data-model-panel]")?.querySelector(".effect-workspace")?.scrollIntoView({block: "start"});
    }
  }));
  all('[data-pick-model]').forEach(link => link.addEventListener('click', event => {
    event.preventDefault(); selectModel(link.dataset.pickModel); navigate('#patterns');
  }));
  all('[data-page-link],.wordmark').forEach(link => link.addEventListener('click', event => {
    event.preventDefault(); navigate(link.hash);
  }));
  document.addEventListener('click', event => {
    const link = event.target.closest('a[href^="#"]');
    if (!link || event.defaultPrevented || link.hasAttribute('data-open-spec') || link.classList.contains('skip')) return;
    const {id} = parseHash(link.hash);
    if (document.getElementById(id)?.closest('.workspace-page')) {
      event.preventDefault();
      if (link.dataset.evidenceModel) selectModel(link.dataset.evidenceModel, {persist: false});
      if (link.dataset.evidenceFeature) selectFeature(link.dataset.evidenceFeature, {persist: false});
      navigate(link.hash);
    }
  });
  all('[data-sort]').forEach(button => button.addEventListener('click', () =>
    sortTable(button.dataset.sort, button.parentElement.getAttribute('aria-sort') === 'ascending' ? -1 : 1)));
  metricControl?.addEventListener('change', () => chooseMetric());
  resourceControl?.addEventListener('change', () => chooseResource());
  costScaleControl?.addEventListener('change', () => chooseResource());
  scoreScaleControl?.addEventListener('change', () => chooseResource());
  classControl?.addEventListener('change', () => chooseClass());
  comparisonControl?.addEventListener('change', () => {
    state.comparisonModelId = comparisonControl.value === state.modelId ? '' : comparisonControl.value;
    comparisonControl.value = state.comparisonModelId; saveState(); emit('comparison-change', {...state});
  });
  all('[data-pair-detail]').forEach(button => button.addEventListener('click', () => {
    all('[data-pair-detail]').forEach(el => el.classList.toggle('is-selected', el === button));
    const detail = document.querySelector('#pair-detail');
    if (detail) detail.textContent = button.dataset.pairDetail;
  }));
  const dialog = document.createElement('dialog');
  dialog.className = 'model-dialog';
  dialog.setAttribute('aria-labelledby', 'model-dialog-title');
  dialog.innerHTML = '<header class="dialog-header"><h2 id="model-dialog-title"></h2>' +
    '<form method="dialog"><button class="dialog-close" aria-label="Close model details">Close</button></form></header>' +
    '<div class="dialog-content"></div>';
  document.body.append(dialog);
  document.addEventListener('click', event => {
    const link = event.target.closest('[data-open-spec]');
    if (!link) return;
    if (typeof dialog.showModal !== 'function') return;
    const source = document.getElementById(link.hash.slice(1))?.querySelector('.model-spec-content');
    if (!source) return;
    event.preventDefault();
    dialog.querySelector('h2').textContent = source.dataset.specLabel;
    dialog.querySelector('.dialog-content').replaceChildren(source.cloneNode(true));
    dialog.showModal(); dialog.querySelector('.dialog-content').scrollTop = 0;
  });
  const printControls = all('select').map(control => {
    const label = document.createElement('span'); label.className = 'print-selection'; control.after(label);
    return {control, label};
  });
  // A PDF contains the selected views; links into hidden tabs or closed details
  // can otherwise become invalid named destinations. Keep external citations.
  const printLinks = new Map();
  function suspendFragmentLinks() {
    all('a[href^="#"]').forEach(link => {
      if (!printLinks.has(link)) printLinks.set(link, link.getAttribute('href'));
      link.removeAttribute('href');
    });
  }
  // Later beforeprint handlers may redraw an SVG. Observe those new links too.
  const printLinkObserver = new MutationObserver(suspendFragmentLinks);
  addEventListener('beforeprint', () => {
    printControls.forEach(({control, label}) => { label.textContent = control.selectedOptions[0]?.textContent || ''; });
    document.activeElement?.blur();
    suspendFragmentLinks();
    printLinkObserver.observe(document.body, {subtree: true, childList: true, attributes: true, attributeFilter: ['href']});
  });
  addEventListener('afterprint', () => {
    printLinkObserver.disconnect();
    printLinks.forEach((href, link) => { if (link.isConnected) link.setAttribute('href', href); });
    printLinks.clear();
  });
  document.querySelector('#print-report')?.addEventListener('click', () => window.print());
  addEventListener('hashchange', () => navigate(location.hash, {persist: false}));
  const helpItems = all('.help');
  function closeHelp(except) {
    helpItems.filter(item => item !== except).forEach(item => {
      item.classList.remove('is-open'); item.querySelector('.help-button').setAttribute('aria-expanded', 'false');
    });
  }
  helpItems.forEach(help => {
    const button = help.querySelector('.help-button'), tooltip = help.querySelector('.help-tip');
    function position() {
      const box = button.getBoundingClientRect(), width = Math.min(360, innerWidth - 24);
      tooltip.style.width = `${width}px`;
      tooltip.style.left = `${Math.max(12, Math.min(box.left, innerWidth - width - 12))}px`;
      tooltip.style.top = `${Math.max(12, Math.min(box.bottom + 8, innerHeight - tooltip.offsetHeight - 12))}px`;
    }
    help.addEventListener('pointerenter', position);
    button.addEventListener('focus', position);
    button.addEventListener('click', () => {
      const opened = button.getAttribute('aria-expanded') !== 'true'; closeHelp(help);
      button.setAttribute('aria-expanded', String(opened)); help.classList.toggle('is-open', opened);
      help.classList.remove('dismissed'); position();
    });
    button.addEventListener('keydown', event => {
      if (event.key === 'Escape') { closeHelp(); help.classList.add('dismissed'); }
    });
    button.addEventListener('blur', () => help.classList.remove('dismissed'));
    help.addEventListener('pointerleave', () => help.classList.remove('dismissed'));
  });
  document.addEventListener('pointerdown', event => { if (!event.target.closest('.help')) closeHelp(); });
  const navigation = document.querySelector('.explorer-nav');
  navigation?.setAttribute('role', 'tablist');
  const orient = () => navigation?.setAttribute('aria-orientation', innerWidth > 760 ? 'vertical' : 'horizontal');
  orient(); addEventListener('resize', orient);
  const tabs = all('[data-page-link]');
  tabs.forEach((tab, index) => {
    tab.id = `tab-${tab.dataset.pageLink}`; tab.setAttribute('role', 'tab');
    tab.setAttribute('aria-controls', tab.dataset.pageLink);
    const page = document.getElementById(tab.dataset.pageLink);
    page?.setAttribute('role', 'tabpanel'); page?.setAttribute('aria-labelledby', tab.id);
    tab.addEventListener('keydown', event => {
      let next;
      if (['ArrowDown', 'ArrowRight'].includes(event.key)) next = (index + 1) % tabs.length;
      if (['ArrowUp', 'ArrowLeft'].includes(event.key)) next = (index + tabs.length - 1) % tabs.length;
      if (event.key === 'Home') next = 0;
      if (event.key === 'End') next = tabs.length - 1;
      if (next !== undefined) { event.preventDefault(); tabs[next].click(); tabs[next].focus(); }
    });
  });
  window.AutoXplainRReport = {navigate, selectModel, selectFeature, getState: () => ({...state})};
  document.addEventListener('axr:model-change', updateComparisonIdentity);
  document.addEventListener('axr:comparison-change', updateComparisonIdentity);
  document.body.classList.add('has-js');
  selectModel(state.modelId, {persist: false}); chooseMetric({persist: false});
  const initialHash = location.hash;
  navigate(initialHash, {focus: false, persist: false});
  const initialTarget = document.getElementById(parseHash(initialHash).id);
  if (initialTarget?.closest('.workspace-page') && !initialTarget.classList.contains('workspace-page')) {
    requestAnimationFrame(() => requestAnimationFrame(() => {
      if (location.hash === initialHash) showPage(initialTarget.closest('.workspace-page')?.id, initialTarget, true);
    }));
  }
})();
