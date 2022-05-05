const input = document.getElementById('treegraph-json').innerText;
const data = JSON.parse(input);
const svg = drawTreeGraph(data.tree, {
  children : datum => datum.children,
  label : datum => datum.name,
  width : document.body.clientWidth,
  calls : data.calls,
  types : data.types,
});
d3.select("#download").on("click", function() {
  const content = svg.outerHTML;
  const href = 'data:application/octet-stream;base64,' + btoa(content);
  d3.select(this).attr("href", href).attr("download", "calligraphy.svg")
})
document.getElementById('view').appendChild(svg);
