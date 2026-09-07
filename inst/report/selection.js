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
  var printing = false;
  function layoutPlots() {
    forEach(section.querySelectorAll(".selection-plot"), function (svg) {
      if (!svg.getBoundingClientRect().width) return;
      var width = printing ? 600 : svg.getBoundingClientRect().width;
      svg.style.width = printing ? "600px" : "100%";
      svg.style.maxWidth = printing ? "100%" : "1100px";
      var narrow = width < 650;
      var left = narrow ? 14 : Math.min(285, width * 0.34);
      var right = width - (narrow ? 18 : 165);
      var rowHeight = narrow ? 58 : 34;
      var count = Number(svg.getAttribute("data-rows"));
      var height = 70 + rowHeight * count;
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
        var y = 31 + Number(row.getAttribute("data-row")) * rowHeight;
        var label = row.querySelector(".selection-row-label"), span = label.querySelector("tspan");
        if (!span.dataset.fullText) span.dataset.fullText = span.textContent;
        span.textContent = span.dataset.fullText;
        label.setAttribute("y", y + (narrow ? -8 : 4));
        var available = narrow ? width - 16 : left - 20;
        while (span.getComputedTextLength() > available && span.textContent.length > 5) {
          span.textContent = span.textContent.replace(/.?…?$/, "…");
        }
        forEach(row.querySelectorAll("circle"), function (point) {
          point.setAttribute("cx", x(point.getAttribute("data-value")));
          point.setAttribute("cy", y + (narrow ? 12 : 0));
        });
        var role = row.querySelector(".selection-role");
        role.style.display = narrow ? "none" : "";
        role.setAttribute("x", right + 15); role.setAttribute("y", y + 4);
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
      if (!detail) return;
      event.preventDefault();
      forEach(details, function (other) { other.hidden = other !== detail; other.open = other === detail; });
      forEach(section.querySelectorAll("[data-selection-inspect]"), function (other) {
        other.setAttribute("aria-expanded", String(other === node));
      });
      detail.querySelector("summary").focus();
      detail.scrollIntoView({block: "nearest"});
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
  window.addEventListener("afterprint", function () { printing = false; layoutPlots(); });
  filter();
}());
