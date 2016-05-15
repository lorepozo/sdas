/* graph drawing */

var width;
var height = window.innerHeight/1.6;

// Node and edge
var radius = 25;        // Radius of nodes
var arrowSize = 100;    // Size of directed edge arrow
var curveWidth = 30;    // Curve width for curved edges
var angle = Math.PI/2.0;

// Styling
var themeColor = "black";
var themeFontSize = "12pt";
var themeEdgeWeightFontSize = "8pt";
var themeChangedFontSize = "16pt";
var themeEdgeChangedFontSize = themeFontSize;
var themeFontWeight = "bold";

DrawingArea = function(_parentElement) {
    this.parentElement = _parentElement;
    this.initVis();
};

DrawingArea.prototype.initVis = function() {
    var vis = this;

    width = document.getElementById(vis.parentElement).getBoundingClientRect().width;
    vis.width = width;
    vis.height = height;

    vis.svg = d3.select("#" + vis.parentElement).append("svg")
        .attr("width", vis.width)
        .attr("height", vis.height)
        .attr("id", "graph-svg");

    vis.bounds = document.getElementById(vis.parentElement).getBoundingClientRect();
    vis.centerX = vis.width / 2.0;
    vis.centerY = vis.height / 2.0;

    vis.svg.append("g").append("text")
        .attr("dx", 10)
        .attr("dy", 30)
        .attr("font-size", "24pt")
        .attr("id", "stepCounter")
        .text("0");
    $("#stepCounter").hide();

    vis.g = vis.svg.append("g")
        .attr("width", vis.width)
        .attr("height", vis.height)
        .attr("transform", "translate(" + vis.centerX + "," + vis.centerY + ")");

    vis.nodeCounter = 0;
};

DrawingArea.prototype.removeAllNodes = function() {
    var vis = this;
    $("circle[id^=n]").each(function() {
        var id = $(this).attr("id");
        removeEdges(id);
        $(this).parent().remove();
        $("#t" + id).remove();
        removeNodeFromMatrix(parseInt(id.slice(1)));
        changeNodeNumbering(parseInt(id.slice(1)));
        vis.nodeCounter--;
    });
};


// Function for adding a node to the middle of the drawing area
function addNode(vis) {
    var position = calcNextXY(100);

    var node = vis.svg.append("g")
        .attr("height", 30)
        .attr("width", 30)
        .attr("transform", "translate(" + vis.centerX + "," + vis.centerY + ")");

    node.append("text")
        .attr("text-anchor", "middle")
        .attr("dx", position[0])
        .attr("dy", position[1] + 5)
        .attr("font-size", themeFontSize)
        .attr("font-weight", themeFontWeight)
        .attr("fill", themeColor)
        .attr("id", "tn" + vis.nodeCounter)
        .text(vis.nodeCounter);

    node.append("text")
        .attr("text-anchor", "middle")
        .attr("dy", position[1]-radius)
        .attr("dx", position[0]-radius)
        .attr("font-size", themeChangedFontSize)
        .attr("font-weight", themeFontWeight)
        .attr("id", "nodeText" + vis.nodeCounter);

    var drag = d3.behavior.drag()
        .on("drag", dragMove)
        .on("dragstart", startDrag)
        .on("dragend", endDrag);

    node.append("circle")
        .attr("r", radius)
        .attr("cx", position[0])
        .attr("cy", position[1])
        .attr("id", "n" + vis.nodeCounter)
        .attr("fill", themeColor)
        .attr("stroke", themeColor)

        .attr("fill-opacity", 0)

        .on("click", function() {
            if (!addMode) {
                var id = $(this).attr("id");
                removeEdges(id);
                $(this).parent().remove();
                $("#t" + id).remove();
                removeNodeFromMatrix(parseInt(id.slice(1)));
                changeNodeNumbering(parseInt(id.slice(1)));
                vis.nodeCounter--;
            }
        })
        .call(drag);

    vis.nodeCounter++;
    addNewNodeToMatrix();
};

// Function for adding an edge between two already drawn nodes
function addEdge(vis, directed) {
    var endNode = parseInt($("#endNode").val());
    var startNode = parseInt($("#startNode").val());
    var curve = false;
    var calcCoord = calcXY(startNode, endNode);

    var weight = ($("#weight").val());
    if (weight == "Weight") {
        weight = NaN;
    } else {
        weight = parseInt(weight);
    }

    if (directed){
        if(checkIfUndirected(startNode, endNode)) {
            removeEdge(startNode, endNode, false);
        } else if (checkIfUndirected(endNode, startNode)) {
            removeEdge(endNode, startNode, false);
        } else if (checkIfDirected(startNode, endNode)) {
            updateWeight(startNode, endNode, weight, true);
            return;
        } else if (checkIfDirected(endNode, startNode)) {
            if ((getEdgeWeight(endNode, startNode) == weight) && !isNaN(weight) && (weight != 0)) {
                removeEdge(endNode, startNode, true);
                directed = false;
                curve = false;
            } else {
                setPathCurved(endNode, startNode);
                curve = true;
            }
        } else {
            curve = false;
        }
    } else {
        curve = false;
        if(checkIfUndirected(startNode, endNode)) {
            updateWeight(startNode, endNode, weight, false);
            return;
        }
        checkIfDirected(startNode, endNode) && removeEdge(startNode, endNode, true);
        checkIfDirected(endNode, startNode) && removeEdge(endNode, startNode, true);
    }
    
    if (directed) {
        addDirectedEdge(vis, startNode, endNode, calcCoord, weight, curve);
    } else {
        addUndirectedEdge(vis, startNode, endNode, calcCoord, weight);
    }
};


function addUndirectedEdge(vis, startNode, endNode, calcCoord, weight) {
    addEdgeToMatrix(startNode, endNode, weight);
    addEdgeToMatrix(endNode, startNode, weight);

    vis.g.append("path")
        .attr("d", function(){
            if(startNode == endNode){
                return calcSelfEdgeCoords(startNode);
            }
            return "M " + calcCoord.x1 + " " + calcCoord.y1 + " L " + calcCoord.x2 + " " + calcCoord.y2;
        })
        .attr("class", "edge")
        .attr("id", "uln" + startNode + "-n" + endNode)
        .on("click", function() {
            if(!addMode) {
                var id = $(this).attr("id");
                var start = id.slice(3, id.indexOf("-"));
                var end = id.slice(id.indexOf("-") + 2);
                removeEdge(start, end, false);
            }
        });

    vis.g.append("text")
        .attr("text-anchor", "middle")
        .attr("transform",
            (startNode == endNode) ? calcSelfEdgeTextPosition(startNode,60) : calcEdgeTextPosition(calcCoord, 15))
        .attr("font-size", themeEdgeChangedFontSize)
        .attr("font-weight", themeFontWeight)
        .attr("id", "tn" + startNode + "-n" + endNode);

    vis.g.append("text")
        .attr("text-anchor", "middle")
        .attr("transform",
            (startNode == endNode) ? calcSelfEdgeTextPosition(startNode,32) : calcStaticEdgeTextPosition(calcCoord, 10))
        .attr("id", "stn" + startNode + "-n" + endNode)
        .attr("font-size", themeEdgeWeightFontSize)
        .text(isNaN(weight) ? "" : weight);
}

function addDirectedEdge(vis, startNode, endNode, calcCoord, weight, curve) {
    setDirected(startNode, endNode);
    addEdgeToMatrix(startNode, endNode, weight);

    vis.g.append("path")
        .attr("class", "edge")
        .attr("id", "dln" + startNode + "-n" + endNode)
        .on("click", function() {
            if(!addMode) {
                var id = $(this).attr("id");
                var start = id.slice(3, id.indexOf("-"));
                var end = id.slice(id.indexOf("-") + 2);
                removeEdge(start, end, true);
            }
        });

    vis.g.append("text")
        .attr("text-anchor", "middle")
        .attr("fill", themeColor)
        .attr("font-size", themeEdgeChangedFontSize)
        .attr("font-weight", themeFontWeight)
        .attr("id", "tn" + startNode + "-n" + endNode);

    vis.g.append("text")
        .attr("text-anchor", "middle")
        .attr("transform",
            (startNode == endNode) ? calcSelfEdgeTextPosition(startNode,32) : calcStaticEdgeTextPosition(calcCoord, 10))
        .attr("font-size", themeEdgeWeightFontSize)
        .attr("id", "stn" + startNode + "-n" + endNode)
        .text(isNaN(weight) ? "" : weight);

    vis.g.append("svg:path")
        .attr("d", d3.svg.symbol().type("triangle-up").size(arrowSize))
        .attr("id", "an" + startNode + "-n" + endNode);

    if(startNode != endNode) {
        curve ? setPathCurved(startNode, endNode) : setPathStraight(startNode, endNode, true);
    } else {
        setPathSelf(startNode, true);
    }

}

function setPathSelf(node, directed) {
    if (directed){
        d3.select("#dln" + node + "-n" + node)
            .attr("d", calcSelfEdgeCoords(node));
        d3.select("#an" + node + "-n" + node)
            .attr("transform", calcSelfEdgeTextPosition(node, 49) + ", rotate(90)");
    } else {
        d3.select("#uln" + node + "-n" + node)
            .attr("d", calcSelfEdgeCoords(node));
    }
    d3.select("#tn" + node + "-n" + node)
        .attr("transform", calcSelfEdgeTextPosition(node, 60));
    d3.select("#stn" + node + "-n" + node)
        .attr("transform", calcSelfEdgeTextPosition(node, 32));
}

function setPathCurved(startNode, endNode) {
    var calcCoord = calcXY(startNode, endNode);

    d3.select("#dln" + startNode + "-n" + endNode)
        .attr("d", calcAnglePoints(calcCoord, curveWidth));

    d3.select("#an" + startNode + "-n" + endNode)
        .attr("transform", calcArrowPosition(calcCoord, 22));

    d3.select("#tn" + startNode + "-n" + endNode)
        .attr("transform", calcEdgeTextPosition(calcCoord, 40));

    d3.select("#stn" + startNode + "-n" + endNode)
        .attr("transform", calcStaticEdgeTextPosition(calcCoord, -10));
}

function setPathStraight(startNode, endNode, directed) {
    var calcCoord = calcXY(startNode, endNode);

    if  (directed) {
        d3.select("#dln" + startNode + "-n" + endNode)
            .attr("d", "M " + calcCoord.x1 + " " + calcCoord.y1 + " L " + calcCoord.x2 + " " + calcCoord.y2);
        d3.select("#an" + startNode + "-n" + endNode)
            .attr("transform", calcArrowPosition(calcCoord, 0));
    } else {
        d3.select("#uln" + startNode + "-n" + endNode)
            .attr("d", "M " + calcCoord.x1 + " " + calcCoord.y1 + " L " + calcCoord.x2 + " " + calcCoord.y2);
    }
    d3.select("#tn" + startNode + "-n" + endNode)
        .attr("transform", calcEdgeTextPosition(calcCoord, 15));

    d3.select("#stn" + startNode + "-n" + endNode)
        .attr("transform", calcStaticEdgeTextPosition(calcCoord, 15));
}


function dragMove() {
    if (!addMode) {return;}
    var circle = d3.select(this);
    circle
        .attr("cx", Math.max(Math.min(d3.event.x, (width/2.0) - radius - 7), -(width/2.0) + radius + 1))
        .attr("cy", Math.max(Math.min(d3.event.y, (height/2.0) - radius - 7), -(height/2.0) + radius + 1));

    var circleNo = circle.attr("id").slice(1);
    
    var directed_start = "[id^=dl" + circle.attr("id") + "-]";
    var directed_end = "[id$=-" + circle.attr("id") + "]:not([id^=a]):not([id^=t]):not([id^=u])";
    var undirected_start = "[id^=ul" + circle.attr("id") + "-]";
    var undirected_end = "[id$=-" + circle.attr("id") + "]:not([id^=a]):not([id^=t]):not([id^=d])";

    var whereStart = d3.selectAll(undirected_start);
    for (var i = 0; i < whereStart[0].length; i++) {
        var line = d3.select(whereStart[0][i]);
        var endId = line.attr("id").slice(line.attr("id").indexOf("-") + 2);
        (circleNo == endId) ? setPathSelf(circleNo, false) : setPathStraight(circleNo, endId);
    };

    var whereEnd = d3.selectAll(undirected_end);
    for (var i = 0; i < whereEnd[0].length; i++) {
        var line = d3.select(whereEnd[0][i]);
        var startId = line.attr("id").slice(3,line.attr("id").indexOf("-"));
        (circleNo == startId) ? setPathSelf(circleNo, false) : setPathStraight(startId, circleNo);
    };

    var whereStart = d3.selectAll(directed_start);
    for (var i = 0; i < whereStart[0].length; i++) {
        var line = d3.select(whereStart[0][i]);
        var endId = line.attr("id").slice(line.attr("id").indexOf("-") + 2);
        if(circleNo == endId) {
            setPathSelf(circleNo, true);
            continue;
        }
        if (checkIfDirected(endId, circleNo)) {
            setPathCurved(circleNo, endId);
        } else {
            setPathStraight(circleNo, endId, true);
        }
    }

    var whereEnd = d3.selectAll(directed_end);
    for (var i = 0; i < whereEnd[0].length; i++) {
        var line = d3.select(whereEnd[0][i]);
        var startId = line.attr("id").slice(3,line.attr("id").indexOf("-"));
        if (circleNo == startId) {
            setPathSelf(circleNo, true);
            continue;
        }
        if (checkIfDirected(circleNo, startId)) {
            setPathCurved(startId, circleNo);
        } else {
            setPathStraight(startId, circleNo, true);
        }
    }
}

function startDrag() {
    if (!addMode) {return;}
    d3.select(this.parentElement).selectAll("text").attr("visibility", "hidden");
    d3.select(this).attr("fill-opacity", 1);
}

function endDrag() {
    if (!addMode) {return;}
    d3.select(this).attr("fill-opacity", 0);
    var textElements = d3.select(this.parentElement).selectAll("text");
    d3.select(textElements[0][0])
        .attr("dx", d3.select(this).attr("cx"))
        .attr("dy", parseFloat(d3.select(this).attr("cy")) + 5)
        .attr("visibility", "visible");

    d3.select(textElements[0][1])
        .attr("dx", parseFloat(d3.select(this).attr("cx")) - radius)
        .attr("dy", parseFloat(d3.select(this).attr("cy")) - radius)
        .attr("visibility", "visible");
}


/**
 * Removes all edges connected to node
 * @param node
 */
function removeEdges(node) {
    var directed_start = "[id^=dl" + node + "-]";
    var directed_end = "[id$=-" + node + "]:not([id^=a]):not([id^=t]):not([id^=u])";
    var undirected_start = "[id^=ul" + node + "-]";
    var undirected_end = "[id$=-" + node + "]:not([id^=a]):not([id^=t]):not([id^=d])";

    var whereStart = d3.selectAll(undirected_start);
    for (var i = 0; i < whereStart[0].length; i++) {
        var line = d3.select(whereStart[0][i]);
        var endId = line.attr("id").slice(line.attr("id").indexOf("-") + 2);
        removeEdge(node.slice(1), endId, false);
    }

    var whereEnd = d3.selectAll(undirected_end);
    for (var i = 0; i < whereEnd[0].length; i++) {
        var line = d3.select(whereEnd[0][i]);
        var startId = line.attr("id").slice(3,line.attr("id").indexOf("-"));
        removeEdge(startId, node.slice(1), false);
    }

    var whereStart = d3.selectAll(directed_start);
    for (var i = 0; i < whereStart[0].length; i++) {
        var line = d3.select(whereStart[0][i]);
        var endId = line.attr("id").slice(line.attr("id").indexOf("-") + 2);
        removeEdge(node.slice(1), endId, true);
    }

    var whereEnd = d3.selectAll(directed_end);
    for (var i = 0; i < whereEnd[0].length; i++) {
        var line = d3.select(whereEnd[0][i]);
        var startId = line.attr("id").slice(3,line.attr("id").indexOf("-"));
        removeEdge(startId, node.slice(1), true);
    }
}


function removeEdge(startNode, endNode, directed) {
    $("#tn" + startNode + "-n" + endNode).remove();
    $("#stn" + startNode + "-n" + endNode).remove();
    if (directed) {
        $("#an" + startNode + "-n" + endNode).remove();
        $("#dln" + startNode + "-n" + endNode).remove();
    } else {
        $("#tn" + endNode + "-n" + startNode).remove();
        $("#stn" + endNode + "-n" + startNode).remove();
        $("#uln" + startNode + "-n" +  endNode).remove();
        $("#uln" + endNode + "-n" + startNode).remove();
        removeEdgeFromMatrix(endNode, startNode);
    }
    removeEdgeFromMatrix(startNode, endNode);
}


function updateWeight(startNode, endNode, newWeight, directed) {
    newWeight = isNaN(newWeight) ? "" : newWeight;
    $("#stn" + startNode + "-n" + endNode).html(newWeight);
    !directed && $("#stn" + endNode + "-n" + startNode).html(newWeight);
}


/**
 * Updates all id's being higher than nodeId
 * @param nodeId
 */
function changeNodeNumbering(nodeId) {
    // Change id of all nodes with id higher than nodeId
    $("[id^=n]circle").each(function() {
        var currentId = parseInt($(this).attr("id").slice(1));
        if (currentId > nodeId) {
            $(this).attr("id", "n" + (currentId - 1));
            $(this).parent().find("#tn" + currentId).attr("id", "tn" + (currentId - 1)).text(currentId-1);
            $(this).parent().find("#nodeText" + currentId).attr("id", "nodeText" + (currentId - 1));
        }
    });

    var edgeTextsDyn = [];
    var edgeTextsStat = [];
    var edgeArrows = [];

    // Change id of all edges with start or end id higher than nodeId
    $("[id^=uln]").each(function() { changeIdEdge(false, this); });
    $("[id^=dln]").each(function() { changeIdEdge(true, this); });

    // Change id of all arrows and edgeTexts with start or end id higher than nodeId
    [edgeTextsDyn, edgeTextsStat, edgeArrows].forEach(function(list) {
        list.forEach(function(item) { $(item[0]).attr("id", item[3] + "n" + item[1] + "-n" + item[2]); })
    });

    function changeIdEdge(isDirected, edge) {
        var currId = $(edge).attr("id");
        var start = parseInt(currId.slice(3, currId.indexOf("-")));
        var end = parseInt(currId.slice(currId.indexOf("-") + 2));
        var edgeText1 = $(edge).parent().find("#tn" + start + "-n" + end);
        var edgeText2 = $(edge).parent().find("#stn" + start + "-n" + end);

        if (isDirected) {
            var arrow = $(edge).parent().find("#an" + start + "-n" + end);
        }

        (start > nodeId) && (start--);
        (end > nodeId)   && (end--);
        edgeTextsDyn.push([edgeText1,start,end, "t"]);
        edgeTextsStat.push([edgeText2,start,end, "st"]);

        if (isDirected) {
            edgeArrows.push([arrow,start,end, "a"]);
            $(edge).attr("id", "dln" + start + "-n" + end);
        } else {
            $(edge).attr("id", "uln" + start + "-n" + end);
        }
    }
}

