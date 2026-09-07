(function () {
  "use strict";
  var section = document.getElementById("selection");
  var input = document.getElementById("selection-family-filter");
  var source = document.getElementById("selection-evidence");
  if (!section || !input || !source) return;
  var evidence;
  try { evidence = JSON.parse(source.textContent); } catch (error) { return; }
  if (!Array.isArray(evidence.candidates)) return;
  var status = document.getElementById("selection-visible-count");
  var families = section.querySelectorAll(".selection-family");
  var details = section.querySelectorAll(".selection-candidate");
  function forEach(nodes, action) { Array.prototype.forEach.call(nodes, action); }
  function wrapLabel(label, available) {
    var initial = label.querySelector("tspan");
    var full = label.dataset.fullText || initial.textContent;
    label.dataset.fullText = full;
    forEach(label.querySelectorAll("tspan"), function (span) { span.remove(); });
    var measure = document.createElementNS("http://www.w3.org/2000/svg", "tspan");
    label.appendChild(measure);
    var lines = [], line = "";
    full.split(/\s+/).forEach(function (word) {
      var trial = line ? line + " " + word : word;
      measure.textContent = trial;
      if (line && measure.getComputedTextLength() > available) { lines.push(line); line = word; }
      else line = trial;
    });
    if (line) lines.push(line);
    measure.remove();
    lines.forEach(function (value, index) {
      var span = document.createElementNS("http://www.w3.org/2000/svg", "tspan");
      span.setAttribute("x", "8"); span.setAttribute("dy", index ? "16" : "0");
      span.textContent = value; label.appendChild(span);
    });
    return lines.length;
  }
  var printing = false;
  function layoutPlots() {
    forEach(section.querySelectorAll(".selection-plot"), function (svg) {
      if (!svg.getBoundingClientRect().width) return;
      var width = printing ? 600 : svg.getBoundingClientRect().width;
      svg.style.width = printing ? "600px" : "100%";
      svg.style.maxWidth = printing ? "100%" : "1100px";
      var narrow = width < 650;
      var left = narrow ? 14 : Math.min(285, width * 0.34);
      var right = width - (narrow ? 18 : 185);
      var cursor = 16;
      forEach(svg.querySelectorAll(".selection-plot-row"), function (row) {
        var label = row.querySelector(".selection-row-label");
        var lineCount = wrapLabel(label, narrow ? width - 16 : left - 20);
        var rowHeight = narrow ? lineCount * 16 + 56 : Math.max(38, lineCount * 16 + 12);
        var y = narrow ? cursor + lineCount * 16 + 36 : cursor + rowHeight / 2;
        label.setAttribute("y", narrow ? cursor + 14 : y + 4 - (lineCount - 1) * 8);
        row.dataset.layoutY = y;
        var hit = row.querySelector(".selection-row-hit");
        hit.setAttribute("y", cursor); hit.setAttribute("width", width); hit.setAttribute("height", rowHeight - 2);
        var role = row.querySelector(".selection-role");
        var score = role.querySelector(".selection-score-label"), state = role.querySelector(".selection-status-label");
        role.setAttribute("x", narrow ? 8 : right + 15);
        role.setAttribute("y", narrow ? cursor + lineCount * 16 + 18 : y - 3);
        score.setAttribute("x", narrow ? 8 : right + 15);
        state.setAttribute("x", narrow ? 16 + score.getComputedTextLength() : right + 15);
        state.setAttribute("dy", narrow ? 0 : 15);
        cursor += rowHeight;
      });
      var height = cursor + 54;
      var min = Number(svg.getAttribute("data-min")), max = Number(svg.getAttribute("data-max"));
      function x(value) { return left + (Number(value) - min) / (max - min) * (right - left); }
      svg.setAttribute("viewBox", "0 0 " + width + " " + height);
      svg.style.height = height + "px";
      var ticks = svg.querySelectorAll(".selection-axis-tick");
      forEach(ticks, function (tick) {
        var index = Array.prototype.indexOf.call(ticks, tick);
        tick.style.display = narrow && ticks.length > 3 && index % 2 ? "none" : "";
        tick.setAttribute("x", x(tick.getAttribute("data-value")));
        tick.setAttribute("y", height - 27);
      });
      forEach(svg.querySelectorAll(".selection-grid, .selection-cutoff"), function (line) {
        var coordinate = x(line.getAttribute("data-value"));
        line.setAttribute("x1", coordinate); line.setAttribute("x2", coordinate);
        line.setAttribute("y2", height - 48);
      });
      forEach(svg.querySelectorAll(".selection-plot-row"), function (row) {
        var y = Number(row.dataset.layoutY);
        forEach(row.querySelectorAll("circle"), function (point) {
          point.setAttribute("cx", x(point.getAttribute("data-value")));
          point.setAttribute("cy", y);
        });
      });
      var axis = svg.querySelector(".selection-axis-label");
      axis.setAttribute("x", (left + right) / 2); axis.setAttribute("y", height - 5);
    });
  }
  function filter() {
    var family = input.value;
    forEach(families, function (node) {
      node.hidden = family !== "all" && node.getAttribute("data-selection-family") !== family;
    });
    forEach(details, function (node) { node.hidden = true; node.open = false; });
    forEach(section.querySelectorAll("[data-selection-inspect]"), function (node) {
      node.setAttribute("aria-expanded", "false");
    });
    forEach(section.querySelectorAll("[data-selection-pick-family]"), function (node) {
      node.setAttribute("aria-pressed", String(node.getAttribute("data-selection-pick-family") === family));
    });
    var count = evidence.candidates.filter(function (candidate) {
      return family === "all" || candidate.family === family;
    }).length;
    status.textContent = count + " configuration" + (count === 1 ? "" : "s");
    layoutPlots();
  }
  function inspectCandidate(detail, selectFamily) {
    if (!detail || !section.contains(detail) || !detail.classList.contains("selection-candidate")) return false;
    if (selectFamily && input.value !== detail.dataset.selectionFamily) {
      input.value = detail.dataset.selectionFamily;
      filter();
    }
    forEach(details, function (other) { other.hidden = other !== detail; other.open = other === detail; });
    forEach(section.querySelectorAll("[data-selection-inspect]"), function (other) {
      other.setAttribute("aria-expanded", String(other.getAttribute("data-selection-inspect") === detail.id));
    });
    return true;
  }
  section.addEventListener("axr:inspect", function (event) { inspectCandidate(event.target, true); });
  forEach(section.querySelectorAll("[data-selection-pick-family]"), function (node) {
    node.addEventListener("click", function () {
      input.value = node.getAttribute("data-selection-pick-family");
      filter();
    });
  });
  forEach(section.querySelectorAll("[data-selection-inspect]"), function (node) {
    node.setAttribute("aria-controls", node.getAttribute("data-selection-inspect"));
    node.addEventListener("click", function (event) {
      var detail = document.getElementById(node.getAttribute("data-selection-inspect"));
      if (!inspectCandidate(detail, false)) return;
      event.preventDefault();
      detail.querySelector("summary").focus();
      detail.scrollIntoView({block: "nearest"});
    });
  });
  forEach(details, function (detail) {
    detail.addEventListener("toggle", function () {
      forEach(section.querySelectorAll("[data-selection-inspect]"), function (link) {
        if (link.getAttribute("data-selection-inspect") === detail.id) {
          link.setAttribute("aria-expanded", String(detail.open && !detail.hidden));
        }
      });
    });
  });
  section.querySelector(".selection-filter").hidden = false;
  input.addEventListener("change", filter);
  window.addEventListener("resize", layoutPlots);
  if (typeof ResizeObserver !== "undefined") new ResizeObserver(layoutPlots).observe(section);
  window.addEventListener("beforeprint", function () {
    printing = true;
    layoutPlots();
  });
  window.addEventListener("afterprint", function () {
    printing = false;
    layoutPlots();
  });
  filter();
  // Shared navigation initializes before this module; replay a direct fold link
  // after family filtering so its destination remains visible on a fresh load.
  try {
    inspectCandidate(document.getElementById(decodeURIComponent(location.hash.slice(1).split("?")[0])), true);
  } catch (error) { /* An invalid fragment is not a candidate destination. */ }
}());
