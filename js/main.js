const input = document.getElementById('treegraph-json').innerText;
const data = JSON.parse(input);
const svg = drawTreeGraph(data.tree, {
  children : datum => datum.children,
  label : datum => datum.name,
  width : document.body.clientWidth,
  calls : data.calls,
  types : data.types,
});
document.getElementById('view').appendChild(svg);
