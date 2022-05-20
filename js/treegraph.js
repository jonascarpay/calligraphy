const drawTreeGraph = (function() {
  const gradientPath = (function() {
    class Path {
      constructor(_) {
        this._ = _;
        this._m = undefined;
      }
      moveTo(x, y) {
        this._ = [];
        this._m = [ x, y ];
      }
      lineTo(x, y) { this._.push(new Line(this._m, this._m = [ x, y ])); }
      bezierCurveTo(ax, ay, bx, by, x, y) {
        this._.push(new BezierCurve(this._m, [ ax, ay ], [ bx, by ],
                                    this._m = [ x, y ]));
      }
      * split(k = 0) {
        const n = this._.length;
        const i = Math.floor(n / 2);
        const j = Math.ceil(n / 2);
        const a = new Path(this._.slice(0, i));
        const b = new Path(this._.slice(j));
        if (i !== j) {
          const [ab, ba] = this._[i].split();
          a._.push(ab);
          b._.unshift(ba);
        }
        if (k > 1) {
          yield* a.split(k - 1);
          yield* b.split(k - 1);
        } else {
          yield a;
          yield b;
        }
      }
      toString() { return this._.join(""); }
    }
    class Line {
      constructor(a, b) {
        this.a = a;
        this.b = b;
      }
      split() {
        const {a, b} = this;
        const m = [ (a[0] + b[0]) / 2, (a[1] + b[1]) / 2 ];
        return [ new Line(a, m), new Line(m, b) ];
      }
      toString() { return `M${this.a}L${this.b}`; }
    }
    const BezierCurve = (function() {
      const l1 = [ 4 / 8, 4 / 8, 0 / 8, 0 / 8 ];
      const l2 = [ 2 / 8, 4 / 8, 2 / 8, 0 / 8 ];
      const l3 = [ 1 / 8, 3 / 8, 3 / 8, 1 / 8 ];
      const r1 = [ 0 / 8, 2 / 8, 4 / 8, 2 / 8 ];
      const r2 = [ 0 / 8, 0 / 8, 4 / 8, 4 / 8 ];

      function dot([ ka, kb, kc, kd ], {a, b, c, d}) {
        return [
          ka * a[0] + kb * b[0] + kc * c[0] + kd * d[0],
          ka * a[1] + kb * b[1] + kc * c[1] + kd * d[1]
        ];
      }

      return class BezierCurve {
        constructor(a, b, c, d) {
          this.a = a;
          this.b = b;
          this.c = c;
          this.d = d;
        }
        split() {
          const m = dot(l3, this);
          return [
            new BezierCurve(this.a, dot(l1, this), dot(l2, this), m),
            new BezierCurve(m, dot(r1, this), dot(r2, this), this.d)
          ];
        }
        toString() { return `M${this.a}C${this.b},${this.c},${this.d}`; }
      };
    })();
    const line = d3.line().curve(d3.curveBundle).x(d => d.y).y(d => d.x);
    return ([ source, target ]) => {
      const p = new Path;
      line.context(p)(source.path(target));
      return p;
    };
  })();
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

  // draw non-hierarchical links between arbitrary nodes
  const drawLinks = (svg, root, outgoing, {color, strokeOpacity}) => {
    const k = 6;
    return svg.append("g")
        .attr("stroke-opacity", strokeOpacity)
        .attr("fill", "none")
        .selectAll("path")
        .data(d3.transpose(root.descendants()
                               .flatMap(d => d[outgoing].map(gradientPath))
                               .map(path => Array.from(path.split(k)))))
        .join("path")
        .style("mix-blend-mode", "darken")
        .attr("stroke", (d, i) => color(i / ((1 << k) - 1)))
        .attr("d", d => d.join(""));
  };
  const drawSimpleLinks =
      (svg, root, outgoing, {stroke, strokeOpacity}) => {
        // the x and y coords are transposed here
        // because we want the tree depth to grow to the right
        const line = d3.line().curve(d3.curveBundle).x(d => d.y).y(d => d.x);
        return svg.append("g")
            .attr("stroke", stroke)
            .attr("stroke-opacity", strokeOpacity)
            .attr("fill", "none")
            .selectAll("path")
            .data(root.descendants().flatMap(d => d[outgoing]))
            .join("path")
            .attr("d", ([ from, to ]) => line(from.path(to)));
      }

  const drawNode = (svg, root, cfg) => {
    const {
      color,
      r,
      labels,
      halo,
      haloWidth,
      stroke,
      fill,
      selected,
      deselected
    } = cfg;
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
    const fillColor = d => {
      const color = d.children ? stroke : fill;
      d.color = color;
      return color
    };

    node.append("rect")
        .filter(d => shapeOf(d) === 'rect')
        .attr('width', 2 * r)
        .attr('height', 2 * r)
        .attr('x', -r)
        .attr('y', -r)
        .attr("fill", fillColor)
        .each(function(d) { d.shape = this; })

    const triangle = d3.line()([ [ 0, -r ], [ -r, r ], [ r, r ] ]);
    const leftTriangle = d3.line()([ [ -r, 0 ], [ r, -r ], [ r, r ] ]);

    node.append("path")
        .filter(d => shapeOf(d) === 'triangle')
        .attr('d', triangle)
        .attr("fill", fillColor)
        .each(function(d) { d.shape = this; })
    node.append("path")
        .filter(d => shapeOf(d) === 'left-triangle')
        .attr('d', leftTriangle)
        .attr("fill", fillColor)
        .each(function(d) { d.shape = this; })
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
        .attr("fill", fillColor)
        .each(function(d) { d.shape = this; })

    node.append("circle")
        .filter(d => shapeOf(d) === 'ring')
        .attr("fill", "none")
        .attr("stroke", fillColor)
        .each(function(d) { d.shape = this; })
        .attr("r", r);

    node.append("circle")
        .filter(d => shapeOf(d) === 'circle')
        .attr("fill", fillColor)
        .each(function(d) { d.shape = this; })
        .attr("r", r);

    node.append("text")
        .attr("fill", color)
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
    const strokeOpacity = 0.4;
    d3.select(this).attr("font-weight", "bold");
    d3.selectAll(d.incoming.map(d => d.path))
        .attr("stroke", "red")
        .attr("stroke-opacity", strokeOpacity)
        .attr("stroke-width", 2)
        .raise();
    d3.selectAll(d.incoming.map(([ d ]) => d.shape)).attr("fill", "red")
    d3.selectAll(d.incoming.map(([ d ]) => d.text)).attr("font-weight", "bold");
    d3.selectAll(d.outgoing.map(d => d.path))
        .attr("stroke", "green")
        .attr("stroke-opacity", strokeOpacity)
        .attr("stroke-width", 2)
        .raise();
    d3.selectAll(d.outgoing.map(([, d ]) => d.shape)).attr("fill", "green")
    d3.selectAll(d.outgoing.map(([, d ]) => d.text))
        .attr("font-weight", "bold");

    d3.selectAll(d.intype.map(d => d.typePath))
        .attr("stroke", "magenta")
        .attr("stroke-opacity", strokeOpacity)
        .attr("stroke-width", 2)
        .raise();
    d3.selectAll(d.intype.map(([ d ]) => d.shape)).attr("fill", "magenta")
    d3.selectAll(d.intype.map(([ d ]) => d.text)).attr("font-weight", "bold");
    d3.selectAll(d.outtype.map(d => d.typePath))
        .attr("stroke", "cyan")
        .attr("stroke-opacity", strokeOpacity)
        .attr("stroke-width", 2)
        .raise();
    d3.selectAll(d.outtype.map(([, d ]) => d.shape)).attr("fill", "cyan")
    d3.selectAll(d.outtype.map(([, d ]) => d.text)).attr("font-weight", "bold");
  }

  const deselected = (color) => {
    return function(event, d) {
      const setFill = d => d.color;
      d3.select(this).attr("font-weight", null);
      d3.selectAll(d.incoming.map(d => d.path))
          .attr("stroke", null)
          .attr("stroke-opacity", 0.2)
          .attr("stroke-width", null);
      d3.selectAll(d.incoming.map(([ d ]) => d.shape)).attr("fill", setFill)
      d3.selectAll(d.incoming.map(([ d ]) => d.text)).attr("font-weight", null);
      d3.selectAll(d.outgoing.map(d => d.path))
          .attr("stroke", null)
          .attr("stroke-opacity", 0.2)
          .attr("stroke-width", null);
      d3.selectAll(d.outgoing.map(([, d ]) => d.shape)).attr("fill", setFill)
      d3.selectAll(d.outgoing.map(([, d ]) => d.text))
          .attr("font-weight", null);
      d3.selectAll(d.intype.map(d => d.typePath))
          .attr("stroke", null)
          .attr("stroke-opacity", 0.2)
          .attr("stroke-width", null);
      d3.selectAll(d.intype.map(([ d ]) => d.shape)).attr("fill", setFill)
      d3.selectAll(d.intype.map(([ d ]) => d.text)).attr("font-weight", null);
      d3.selectAll(d.outtype.map(d => d.typePath))
          .attr("stroke", null)
          .attr("stroke-opacity", 0.2)
          .attr("stroke-width", null);
      d3.selectAll(d.outtype.map(([, d ]) => d.shape)).attr("fill", setFill)
      d3.selectAll(d.outtype.map(([, d ]) => d.text)).attr("font-weight", null);
    };
  };

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
    strokeWidth = 2,
    strokeOpacity = 0.05,
    strokeLinejoin,
    strokeLinecap,
    light = {
      color : "#000",
      fill : "#999",
      stroke : "#555",
      halo : "#fff",
    },
    dark = {
      color : "#fff",
      fill : "#bbb",
      stroke : "#777",
      halo : "#000",
    },
    haloWidth = 1,
  } = {}) {
    let colors = light;
    if (window.matchMedia &&
        window.matchMedia('(prefers-color-scheme: dark)').matches) {
      colors = dark;
    }
    const {color, fill, stroke, halo} = colors;
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
      color : d3.interpolateHsl("red", "green"),
      strokeOpacity : 0.3,
    })
    drawSimpleLinks(svg, root, 'outgoing', {
      stroke : "steelblue",
      strokeOpacity : 0.0,
    }).each(function(d) { d.path = this; })
    drawLinks(svg, root, 'outtype', {
      color : d3.interpolateHsl("magenta", "cyan"),
      strokeOpacity : 0.3,
    })
    drawSimpleLinks(svg, root, 'outtype', {
      stroke : "orange",
      strokeOpacity : 0.0,
    }).each(function(d) { d.typePath = this; })
    drawNode(svg, root, {
      r,
      color,
      labels,
      halo,
      haloWidth,
      stroke,
      fill,
      selected,
      deselected : deselected(color)
    })
    return svg.node();
  };
})();
