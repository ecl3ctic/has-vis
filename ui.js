
// Initialize Cola

var d3cola;

function createD3Cola() {
    return cola.d3adaptor().convergenceThreshold(0.1);
}

// Communication stuff

var port = 56250;
var ghci = new WebSocket("ws://localhost:" + port);
var nodes;
var links;

ghci.onopen = function (event) {
    $("#connectionMessage").text("Connected.");
}

ghci.onclose = function (event) {
    clearWindow();
}

// Receive
ghci.onmessage = function (event) {
    var data;
    try {
        data = $.parseJSON(event.data);
    } catch (e) {
        data = event.data;
    }
    if (data instanceof Array) { // Graph data
        // Clear previous graph
        clearGraph();

        // Parse nodes
        nodes = data[0];
        links = [];

        // Process each of the links, hooking up references to their nodes
        data[1].forEach(function(e) {
            var sourceNode = nodes.filter(function(n) {
                return n.id === e.source;
            })[0],
                targetNode = nodes.filter(function(n) {
                    return n.id === e.target;
            })[0];
            if (sourceNode && targetNode) {
              links.push({
                  source: sourceNode,
                  target: targetNode,
                  ptrIndex: e.ptrIndex
              });
            }
        });

        nodes.forEach(function (val, i) {
            // Assign random starting position
            val.x = Math.random() * canvasWidth;
            val.y = Math.random() * canvasHeight;
        });

        // Initialize Cola
        d3cola
            .avoidOverlaps(true)
            .flowLayout('y', 120)
            .size([canvasWidth, canvasHeight])
            .nodes(nodes)
            .links(links)
            .jaccardLinkLengths(120);

        // Create node graphics
        var node = vis.selectAll(".node")
            .data(nodes)
            .enter().append("rect")
            .attr("class", "node")
            .call(d3cola.drag);

        // Create link graphics
        var link = vis.selectAll(".link")
            .data(links)
            .enter().append("path")
            .attr("class", "link");

        // Create label graphics, and calculate node
        // boundaries for use by the layout algorithm.
        var margin = 8, pad = 8;
        var label = vis.selectAll(".label")
            .data(nodes)
            .enter().append("text")
            .attr("class", "label")
            .text(function (d) { return d.name; }) // Label text
            .call(d3cola.drag)
            .each(function (d) {
                var b = this.getBBox(); // Bounding box of rect
                var extra = 2 * margin + 2 * pad;
                d.width = b.width + extra;
                d.height = b.height + extra;
            });

        // Draw lines using a B-spline
        var lineFuncSpline = d3.svg.line()
            .x(function (d) { return d.x; })
            .y(function (d) { return d.y; })
            .interpolate("basis");

        // 10 iter no contraints, 30 iter some, 100 iter all.
        d3cola.start(10, 30, 100).on("tick", function () {
                node.each(function (d) { d.innerBounds = d.bounds.inflate(-margin); })
                    .attr("x", function (d) { return d.innerBounds.x; })
                    .attr("y", function (d) { return d.innerBounds.y; })
                    .attr("width", function (d) { return d.innerBounds.width(); })
                    .attr("height", function (d) { return d.innerBounds.height(); });

                link.attr("d", function (d) {
                    cola.vpsc.makeEdgeBetween(d, d.source.innerBounds, d.target.innerBounds, 5);

                    // Interval between edge x positions
                    var intervalLength = d.source.innerBounds.width() / d.source.ptrCount;
                    var xStart = d.source.innerBounds.x + (0.5 + d.ptrIndex) * intervalLength;
                    var yStart = d.source.innerBounds.y + d.source.innerBounds.height();

                    var lineData;
                    if (d.source === d.target) { // Add an extra control point for loops
                        lineData = [
                          { x: xStart, y: yStart},
                          { x: xStart - 15, y: yStart + 40},
                          { x: xStart + 15, y: yStart + 40},
                          { x: xStart, y: yStart}
                        ];
                    } else {
                        lineData = [
                          { x: xStart, y: yStart},
                          { x: xStart, y: yStart + 20},
                          { x: d.arrowStart.x, y: d.arrowStart.y }
                        ];
                    }
                    return lineFuncSpline(lineData);
                });
                if (isIE()) link.each(function (d) { this.parentNode.insertBefore(this, this) });

                label
                    .attr("x", function (d) { return d.x })
                    .attr("y", function (d) { return d.y + (margin + pad) / 2 });
            });
    } else {
        // If the data is an object with an "action" property, execute the action.
        if (data && data.action) {
            switch (data.action) {
                case "clear":
                    clearGraph();
                    break;
                case "close":
                    clearWindow();
                    break;
                default:
                    console.log("Unknown action: " + data.action);
            }
        } else {
            console.log("Unexpected data: " + data);
        }
    }
}

function clearGraph() {
    if (d3cola) {
        d3cola.stop();
        d3cola.nodes([]).links([]);
    }
    vis.selectAll("*").remove();
    d3cola = createD3Cola();
}

function clearWindow() {
    clearGraph();
    $("body").contents().hide();
    $("body").append("<h1>Connection to the terminal has been lost.</h1>");
    $("body").append("<p>The visualisation may have been closed at the terminal. This window must be closed manually.</p>");
}

// Check whether browser is IE
function isIE() { return ((navigator.appName == 'Microsoft Internet Explorer') || ((navigator.appName == 'Netscape') && (new RegExp("Trident/.*rv:([0-9]{1,}[\.0-9]{0,})").exec(navigator.userAgent) != null))); }
