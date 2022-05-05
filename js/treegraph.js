const drawTreeGraph = (function() {
  const addLinks = (root, edges, incoming, outgoing) => {
    incoming = incoming || 'incoming';
    outgoing = outgoing || 'outgoing';
    const map = new Map(root.descendants().map(d => [d.data.key, d]));
    root.descendants().forEach(d => {
      d[incoming] = [];
      d[outgoing] = [];
    })
    for (const edge of edges) {
      const from = map.get(edge.source);
      const to = map.get(edge.target);
      from[outgoing].push([ from, to ])
    }
    root.descendants().forEach(
        d => { d[outgoing].forEach(o => { o[1][incoming].push(o); }); });
    return root;
  };

  // draw hierarchical links between nodes
  const drawTreeLinks = (svg, root, cfg) => {
    const {stroke, strokeOpacity, strokeLinejoin, strokeLinecap, strokeWidth} =
        cfg;
    svg.append("g")
        .attr("fill", "none")
        .attr("stroke", stroke)
        .attr("stroke-opacity", strokeOpacity)
        .attr("stroke-linecap", strokeLinecap)
        .attr("stroke-linejoin", strokeLinejoin)
        .attr("stroke-width", strokeWidth)
        .selectAll("path")
        .data(root.links())
        .join("path")
        .attr("d", d3.linkHorizontal().x(d => d.y).y(d => d.x));
  };

  // the x and y coords are transposed here
  // because we want the tree depth to grow to the right
  const line = d3.line().curve(d3.curveBundle).x(d => d.y).y(d => d.x);

  // draw non-hierarchical links between arbitrary nodes
  const drawLinks = (svg, root, outgoing, {stroke, strokeOpacity}) =>
      svg.append("g")
          .attr("stroke", stroke)
          .attr("stroke-opacity", strokeOpacity)
          .attr("fill", "none")
          .selectAll("path")
          .data(root.descendants().flatMap(d => d[outgoing]))
          .join("path")
          .style("mix-blend-mode", "darken")
          .attr("d", ([ from, to ]) => line(from.path(to)));

  const drawNode = (svg, root, cfg) => {
    const {r, labels, halo, haloWidth, stroke, fill, selected, deselected} =
        cfg;
    const node = svg.append("g")
                     .selectAll("a")
                     .data(root.descendants())
                     .join("a")
                     .attr("transform", d => `translate(${d.y},${d.x})`);
    const shapeOf = d => d.data.tag === 'module'         ? 'ring'
                         : d.data.type === 'data'        ? 'rect'
                         : d.data.type === 'constructor' ? 'triangle'
                         : d.data.type === 'record'      ? 'left-triangle'
                         : d.data.type === 'type-class'  ? 'star'
                         : d.data.type === 'value'       ? 'circle'
                                                         : 'circle';

    node.append("rect")
        .filter(d => shapeOf(d) === 'rect')
        .attr('width', 2 * r)
        .attr('height', 2 * r)
        .attr('x', -r)
        .attr('y', -r)
        .attr("fill", d => d.children ? stroke : fill)

    const triangle = d3.line()([ [ 0, -r ], [ -r, r ], [ r, r ] ]);
    const leftTriangle = d3.line()([ [ -r, 0 ], [ r, -r ], [ r, r ] ]);

    node.append("path")
        .filter(d => shapeOf(d) === 'triangle')
        .attr('d', triangle)
        .attr("fill", d => d.children ? stroke : fill)
    node.append("path")
        .filter(d => shapeOf(d) === 'left-triangle')
        .attr('d', leftTriangle)
        .attr("fill", d => d.children ? stroke : fill)
    const r1 = r * 1.5;
    const r2 = r1 * 6 / 15;
    const star = d3.radialLine()([
      [ 0, r1 ], [ Math.PI * 0.2, r2 ], [ Math.PI * 0.4, r1 ],
      [ Math.PI * 0.6, r2 ], [ Math.PI * 0.8, r1 ], [ Math.PI * 1, r2 ],
      [ Math.PI * 1.2, r1 ], [ Math.PI * 1.4, r2 ], [ Math.PI * 1.6, r1 ],
      [ Math.PI * 1.8, r2 ], [ Math.PI * 2, r1 ]
    ]);
    node.append("path")
        .filter(d => shapeOf(d) === 'star')
        .attr('d', star)
        .attr("fill", d => d.children ? stroke : fill)

    node.append("circle")
        .filter(d => shapeOf(d) === 'ring')
        .attr("fill", "none")
        .attr("stroke", d => d.children ? stroke : fill)
        .attr("r", r);

    node.append("circle")
        .filter(d => shapeOf(d) === 'circle')
        .attr("fill", d => d.children ? stroke : fill)
        .attr("r", r);

    node.append("text")
        .attr("dy", "0.32em")
        .attr("x", d => d.children ? -6 : 6)
        .attr("text-anchor", d => d.children ? "end" : "start")
        .attr("paint-order", "stroke")
        .attr("stroke", halo)
        .attr("stroke-width", haloWidth)
        .each(function(d) { d.text = this; })
        .on("mouseover", selected)
        .on("mouseout", deselected)
        .text((d, i) => labels[i]);
    return node;
  };

  function selected(event, d) {
    d3.select(this).attr("font-weight", "bold");
    d3.selectAll(d.incoming.map(d => d.path))
        .attr("stroke", "red")
        .attr("stroke-opacity", 1)
        .raise();
    d3.selectAll(d.incoming.map(([ d ]) => d.text))
        .attr("fill", "red")
        .attr("font-weight", "bold");
    d3.selectAll(d.outgoing.map(d => d.path))
        .attr("stroke", "green")
        .attr("stroke-opacity", 1)
        .raise();
    d3.selectAll(d.outgoing.map(([, d ]) => d.text))
        .attr("fill", "green")
        .attr("font-weight", "bold");

    d3.selectAll(d.intype.map(d => d.typePath))
        .attr("stroke", "magenta")
        .attr("stroke-opacity", 1)
        .raise();
    d3.selectAll(d.intype.map(([ d ]) => d.text))
        .attr("fill", "magenta")
        .attr("font-weight", "bold");
    d3.selectAll(d.outtype.map(d => d.typePath))
        .attr("stroke", "cyan")
        .attr("stroke-opacity", 1)
        .raise();
    d3.selectAll(d.outtype.map(([, d ]) => d.text))
        .attr("fill", "cyan")
        .attr("font-weight", "bold");
  }

  function deselected(event, d) {
    d3.select(this).attr("font-weight", null);
    d3.selectAll(d.incoming.map(d => d.path))
        .attr("stroke", null)
        .attr("stroke-opacity", 0.2);
    d3.selectAll(d.incoming.map(([ d ]) => d.text))
        .attr("fill", null)
        .attr("font-weight", null);
    d3.selectAll(d.outgoing.map(d => d.path))
        .attr("stroke", null)
        .attr("stroke-opacity", 0.2);
    d3.selectAll(d.outgoing.map(([, d ]) => d.text))
        .attr("fill", null)
        .attr("font-weight", null);
    d3.selectAll(d.intype.map(d => d.typePath))
        .attr("stroke", null)
        .attr("stroke-opacity", 0.2);
    d3.selectAll(d.intype.map(([ d ]) => d.text))
        .attr("fill", null)
        .attr("font-weight", null);
    d3.selectAll(d.outtype.map(d => d.typePath))
        .attr("stroke", null)
        .attr("stroke-opacity", 0.2);
    d3.selectAll(d.outtype.map(([, d ]) => d.text))
        .attr("fill", null)
        .attr("font-weight", null);
  }

  // compute tree layout
  const layoutTree = (root, {height, width, padding}) => {
    const dx = 10;
    const dy = width / (root.height + padding);
    const tree = d3.tree().nodeSize([ dx, dy ])(root);

    // Center the tree.
    let x0 = Infinity;
    let x1 = -x0;
    root.each(d => {
      if (d.x > x1)
        x1 = d.x;
      if (d.x < x0)
        x0 = d.x;
    });

    // Compute the default height.
    if (height === undefined) {
      height = x1 - x0 + dx * 2;
    }
    return {w : width, h : height, dx, dy, x0, x1, tree};
  };

  return function drawTreeGraph(data, {
    children,
    calls,
    types,
    label,
    width = 640,
    height,
    r = 3,
    padding = 1,
    fill = "#999",
    stroke = "#555",
    strokeWidth = 1.5,
    strokeOpacity = 0.05,
    strokeLinejoin,
    strokeLinecap,
    halo = "#fff",
    haloWidth = 2,
  } = {}) {
    const root = addLinks(addLinks(d3.hierarchy(data, children), calls), types,
                          'intype', 'outtype');

    const labels = root.descendants().map(d => label(d.data, d));
    const {dx, dy, w, h, x0} = layoutTree(root, {width, height, padding});

    const svg =
        d3.create("svg")
            .attr("xmlns", "http://www.w3.org/2000/svg")
            .attr("viewBox", [ -dy * padding / 2, x0 - dx, w, h ])
            .attr("width", w)
            .attr("height", h)
            .attr("style", "max-width: 100%; height: auto; height: intrinsic;")
            .attr("font-family", "sans-serif")
            .attr("font-size", 10);
    drawTreeLinks(
        svg, root,
        {stroke, strokeOpacity, strokeLinejoin, strokeLinecap, strokeWidth})

    drawLinks(svg, root, 'outgoing', {
      stroke : 'steelblue',
      strokeOpacity : 0.2
    }).each(function(d) { d.path = this; })
    drawLinks(svg, root, 'outtype', {stroke : 'orange', strokeOpacity : 0.2})
        .each(function(d) { d.typePath = this; })
    drawNode(svg, root,
             {r, labels, halo, haloWidth, stroke, fill, selected, deselected})
    return svg.node();
  };
})();
