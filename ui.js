
// Initialize Cola

var d3cola;

function createD3Cola() {
    return cola.d3adaptor().convergenceThreshold(0.1);
}

// Communication stuff

var port = 56250;
var ghci = new WebSocket("ws://localhost:" + port);

ghci.onopen = function (event) {
    ghci.send("Connected to GHCi.");
}

// Check whether a string starts with the given prefix
String.prototype.startsWith = function (str) {
    return this.lastIndexOf(str, 0) === 0;
};

var nodes;
var links;

// Receive
ghci.onmessage = function(event) {
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
        linksByID = data[1]; // Before processing
        links = []; // Hold references rather than IDs

        linksByID.forEach(function(e) {
            var sourceNode = nodes.filter(function(n) {
                return n.id === e.source;
            })[0],
                targetNode = nodes.filter(function(n) {
                    return n.id === e.target;
            })[0];
            if (sourceNode && targetNode) {
              links.push({
                  source: sourceNode,
                  target: targetNode
              });
            }
        });

        nodes.forEach(function (val, i) {
            // Assign random starting position
            val.x = Math.random() * canvasWidth;
            val.y = Math.random() * canvasHeight;
        });

        // Give graph info to Cola for solving
        //d3cola.nodes(nodes).links(links);

        d3cola
            .avoidOverlaps(true)
            .flowLayout('x', 150)
            .size([canvasWidth, canvasHeight])
            .nodes(nodes)
            .links(links)
            .jaccardLinkLengths(150);

        // Create link graphics
        var link = vis.selectAll(".link")
            .data(links)
            .enter().append("path")
            .attr("class", "link");

        // Create node graphics
        var margin = 10, pad = 12;
        var node = vis.selectAll(".node")
            .data(nodes)
            .enter().append("rect")
            .attr("class", "node")
            .call(d3cola.drag);

        // Create label graphics, and calculate node
        // boundaries for use by the layout algorithm.
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

        var lineFunction = d3.svg.line()
            .x(function (d) { return d.x; })
            .y(function (d) { return d.y; })
            .interpolate("linear");

        // 10 iter no contraints, 30 iter some, 100 iter all.
        d3cola.start(10, 30, 100).on("tick", function () {
                node.each(function (d) { d.innerBounds = d.bounds.inflate(-margin); })
                    .attr("x", function (d) { return d.innerBounds.x; })
                    .attr("y", function (d) { return d.innerBounds.y; })
                    .attr("width", function (d) { return d.innerBounds.width(); })
                    .attr("height", function (d) { return d.innerBounds.height(); });

                link.attr("d", function (d) {
                    cola.vpsc.makeEdgeBetween(d, d.source.innerBounds, d.target.innerBounds, 5);
                    var lineData = [{ x: d.sourceIntersection.x, y: d.sourceIntersection.y }, { x: d.arrowStart.x, y: d.arrowStart.y }];
                    return lineFunction(lineData);
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
                    // Clear the graph
                    clearGraph();
                    break;
                case "close":
                    window.close();
                default:
                    console.log("Unknown action: " + data.action);
            }
        } else {
            console.log("Unexpected data: " + data);
        }
    }
}

// Burn it all
function clearGraph() {
    if (d3cola) {
        d3cola.stop();
        d3cola.nodes([]).links([]);
    }
    vis.selectAll("*").remove();
    d3cola = createD3Cola();
}

// Check whether browser is IE
function isIE() { return ((navigator.appName == 'Microsoft Internet Explorer') || ((navigator.appName == 'Netscape') && (new RegExp("Trident/.*rv:([0-9]{1,}[\.0-9]{0,})").exec(navigator.userAgent) != null))); }
