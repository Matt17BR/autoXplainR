(() => {
  "use strict";
  let node = document.currentScript.previousElementSibling;
  const chunks = [];
  while (node && node.hasAttribute("data-json-chunk")) {
    chunks.push(node);
    node = node.previousElementSibling;
  }
  if (!node || node.type !== "application/json" || !node.hasAttribute("data-json-chunks")) return;
  chunks.reverse();
  const count = Number(node.dataset.jsonChunks);
  const valid = count > 0 && count === chunks.length && chunks.every((chunk, index) =>
    chunk.type === "application/octet-stream" && chunk.dataset.jsonOwner === node.id &&
    chunk.dataset.jsonChunk === String(index + 1));
  if (!valid) return;
  node.textContent = chunks.map(chunk => chunk.textContent).join("");
  chunks.forEach(chunk => chunk.remove());
  node.removeAttribute("data-json-chunks");
})();
