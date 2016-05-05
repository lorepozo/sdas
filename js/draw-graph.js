/**
 * Created by Moan on 28/04/16.
 */


var width;
var height = window.innerHeight/1.5;
var radius = 15;
var arrowSize = 50;
var curveWidth = 30;
var themeColor = "black";
var addMode = true;

var nodeX, nodeY;
var angle = Math.PI/2.0;

DrawingArea = function(_parentElement) {
    this.parentElement = _parentElement;
    this.initVis();
};

DrawingArea.prototype.initVis = function() {
    var vis = this;

    width = document.getElementById(vis.parentElement).getBoundingClientRect().width;
    vis.width = width;
    vis.height = height;

    vis.margin = {top: 20, right: 20, left: 20, bottom: 20};

    vis.svg = d3.select("#" + vis.parentElement).append("svg")
        .attr("width", vis.width)
        .attr("height", vis.height)
        .attr("id", "graph-svg");

    vis.bounds = document.getElementById(vis.parentElement).getBoundingClientRect();
    vis.centerX = vis.width / 2.0;
    vis.centerY = vis.height / 2.0;

    vis.g = vis.svg.append("g")
        .attr("width", vis.width)
        .attr("height", vis.height)
        .attr("transform", "translate(" + vis.centerX + "," + vis.centerY + ")");

    vis.nodeCounter = 0;

    
};


// Function for adding a node to the middle of the drawing area
function addNode(vis) {

    var nodeText = $("#nodeText").val();
    var textSet = false;
    (nodeText != "Node text") && (textSet = true);
    var position = calcNextXY(100, angle);
    angle = position[2];

    var node = vis.svg.append("g")
        .attr("height", 30)
        .attr("width", 30)
        .attr("transform", "translate(" + vis.centerX + "," + vis.centerY + ")");

    node.append("text")
        .attr("text-anchor", "middle")
        .attr("dx", position[0])
        .attr("dy", position[1] + 5)
        .attr("font-color", themeColor)
        .attr("id", "tn" + vis.nodeCounter)
        .text(vis.nodeCounter);

    console.log(position[0]);
    console.log(position[1]);

    node.append("text")
        .attr("text-anchor", "middle")
        .attr("dy", position[1]-17.5)
        .attr("dx", position[0]-15)
        .attr("font-color", themeColor)
        .attr("id", "nodeText" + vis.nodeCounter);

    textSet && d3.select("#nodeText" + vis.nodeCounter).text(nodeText);

    var drag = d3.behavior.drag()
        .on("drag", dragmove)
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
                removeAllEdges(id);
                $(this).remove();
                $("#t" + id).remove();
                removeNodeFromMatrix(id.slice(1));
                changeNodeNumbering(id.slice(1));
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
        weight = 1.0;
    } else {
        weight = parseFloat(weight);
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
            if (getEdgeWeight(endNode, startNode) == weight) {
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


    if(directed) {
        addDirectedEdge(vis, startNode, endNode, calcCoord, weight, curve);

    } else {
        vis.g.append("path")
            .attr("d", "M " + calcCoord.x1 + " " + calcCoord.y1 + " L " + calcCoord.x2 + " " + calcCoord.y2 )
            .attr("class", "edge")
            .attr("id", "uln" + startNode + "-n" + endNode);

        vis.g.append("text")
            .attr("text-anchor", "middle")
            .attr("transform", calcEdgeTextPosition(calcCoord, 15))
            .attr("font-color", themeColor)
            .attr("id", "tn" + startNode + "-n" + endNode)
            .text(weight);
        addEdgeToMatrix(startNode, endNode, weight);
        addEdgeToMatrix(endNode, startNode, weight);
    }
};

function addDirectedEdge(vis, startNode, endNode, calcCoord, weight, curve) {

    setDirected(startNode, endNode);
    addEdgeToMatrix(startNode, endNode, weight);

    vis.g.append("path")
        .attr("class", "edge")
        .attr("id", "dln" + startNode + "-n" + endNode)
        .on("click", function() {
            !addMode && removeEdge(startNode, endNode, true);
        });

    // Add text
    vis.g.append("text")
        .attr("text-anchor", "middle")
        .attr("font-color", themeColor)
        .attr("id", "tn" + startNode + "-n" + endNode)
        .text(weight);

    // Add arrow
    vis.g.append("svg:path")
        .attr("d", d3.svg.symbol().type("triangle-up").size(arrowSize))
        .attr("id", "an" + startNode + "-n" + endNode);

    // Add line
    curve ? setPathCurved(startNode, endNode) : setPathStraight(startNode, endNode, true);
}

function setPathCurved(startNode, endNode) {
    var calcCoord = calcXY(startNode, endNode);

    d3.select("#dln" + startNode + "-n" + endNode)
        .attr("d", calcAnglePoints(calcCoord));

    d3.select("#an" + startNode + "-n" + endNode)
        .attr("transform", calcArrowPosition(calcCoord, 22));

    d3.select("#tn" + startNode + "-n" + endNode)
        .attr("transform", calcEdgeTextPosition(calcCoord, 40));
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
}


function dragmove() {
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
    for(var i = 0; i < whereStart[0].length; i++) {
        var line = d3.select(whereStart[0][i]);
        var endId = line.attr("id").slice(line.attr("id").indexOf("-") + 2);
        setPathStraight(circleNo, endId);
    };

    var whereEnd = d3.selectAll(undirected_end);
    for(var i = 0; i < whereEnd[0].length; i++) {
        var line = d3.select(whereEnd[0][i]);
        var startId = line.attr("id").slice(3,line.attr("id").indexOf("-"));
        setPathStraight(startId, circleNo);
    };

    var whereStart = d3.selectAll(directed_start);
    for(var i = 0; i < whereStart[0].length; i++) {
        var line = d3.select(whereStart[0][i]);
        var endId = line.attr("id").slice(line.attr("id").indexOf("-") + 2);

        if (checkIfDirected(endId, circleNo)) {
            setPathCurved(circleNo, endId);
        } else {
            setPathStraight(circleNo, endId, true);
        }
    }

    var whereEnd = d3.selectAll(directed_end);
    for(var i = 0; i < whereEnd[0].length; i++) {
        var line = d3.select(whereEnd[0][i]);
        var startId = line.attr("id").slice(3,line.attr("id").indexOf("-"));

        if (checkIfDirected(circleNo, startId)) {
            setPathCurved(startId, circleNo);
        } else {
            setPathStraight(startId, circleNo, true);
        }
    }
}

function startDrag() {
    d3.select(this.parentElement).selectAll("text").attr("visibility", "hidden");
    d3.select(this).attr("fill-opacity", 1);
}

function endDrag() {
    d3.select(this).attr("fill-opacity", 0);
    var textElements = d3.select(this.parentElement).selectAll("text");
    d3.select(textElements[0][0])
        .attr("dx", d3.select(this).attr("cx"))
        .attr("dy", parseFloat(d3.select(this).attr("cy")) + 5)
        .attr("visibility", "visible");

    d3.select(textElements[0][1])
        .attr("dx", parseFloat(d3.select(this).attr("cx")) - 15)
        .attr("dy", parseFloat(d3.select(this).attr("cy")) - 17.5)
        .attr("visibility", "visible");
}

function removeAllEdges(node) {
    var directed_start = "[id^=dl" + node + "-]";
    var directed_end = "[id$=-" + node + "]:not([id^=a]):not([id^=t]):not([id^=u])";
    var undirected_start = "[id^=ul" + node + "-]";
    var undirected_end = "[id$=-" + node + "]:not([id^=a]):not([id^=t]):not([id^=d])";

    var whereStart = d3.selectAll(undirected_start);
    for(var i = 0; i < whereStart[0].length; i++) {
        var line = d3.select(whereStart[0][i]);
        var endId = line.attr("id").slice(line.attr("id").indexOf("-") + 2);
        removeEdge(node.slice(1), endId, false);
    }

    var whereEnd = d3.selectAll(undirected_end);
    for(var i = 0; i < whereEnd[0].length; i++) {
        var line = d3.select(whereEnd[0][i]);
        var startId = line.attr("id").slice(3,line.attr("id").indexOf("-"));
        removeEdge(startId, node.slice(1), false);
    }

    var whereStart = d3.selectAll(directed_start);
    for(var i = 0; i < whereStart[0].length; i++) {
        var line = d3.select(whereStart[0][i]);
        var endId = line.attr("id").slice(line.attr("id").indexOf("-") + 2);
        removeEdge(node.slice(1), endId, true);
    }

    var whereEnd = d3.selectAll(directed_end);
    for(var i = 0; i < whereEnd[0].length; i++) {
        var line = d3.select(whereEnd[0][i]);
        var startId = line.attr("id").slice(3,line.attr("id").indexOf("-"));
        removeEdge(startId, node.slice(1), true);
    }
}

function removeEdge(startNode, endNode, directed) {
    $("#tn" + startNode + "-n" + endNode).remove();
    $("#tn" + endNode + "-n" + startNode).remove();
    if (directed) {
        $("#an" + startNode + "-n" + endNode).remove();
        $("#dln" + startNode + "-n" + endNode).remove();
    } else {
        $("#uln" + startNode + "-n" +  endNode).remove();
        $("#uln" + endNode + "-n" + startNode).remove();
        removeEdgeFromMatrix(endNode, startNode);
    }
    removeEdgeFromMatrix(startNode, endNode);
}


function updateWeight(startNode, endNode, newWeight, directed) {
    if(directed) {
        var textElement = document.getElementById("dln" + startNode + "-n" + endNode).nextElementSibling;
    } else {
        try {
            var textElement = document.getElementById("uln" + startNode + "-n" + endNode).nextElementSibling;
        } catch (TypeError) {
            var textElement = document.getElementById("uln" + endNode + "-n" + startNode).nextElementSibling;
        }
    }
    textElement.innerHTML = newWeight;
}

function changeNodeNumbering(nodeId) {
    var allNodes = d3.selectAll("[id^=n]");
    var allEdges = d3.selectAll("[id*=ln]");

    for (var i = 0; i < allNodes[0].length; i++) {
        var currentNode = $(allNodes[0][i]);
        var currentId = currentNode.attr("id").slice(1);
        if (currentId > nodeId) {
            currentNode.attr("id", "n" + (currentId - 1));
            var nodeInnerText = $("#tn" + currentId);
            $("nodeText" + currentId).attr("id", "nodeText" + (currentId - 1));
            nodeInnerText.text(currentId-1);
            nodeInnerText.attr("id", "tn" + (currentId-1));

        }
    }

    for (var i = 0; i < allEdges[0].length; i++) {
        var currentEdge = $(allEdges[0][i]);

        var currentId = currentEdge.attr("id");
        if (currentId.slice(0,1) == "d"){
            var currentArrow = $("#a" + currentId.slice(2));
            var dir = true;
        }
        var startId = currentId.slice(3, currentId.indexOf("-"));
        var endId = currentId.slice(currentId.indexOf("-")+2);
        var currentEdgeText = $("#tn" + startId + "-n" + endId);
        (startId > nodeId) && (startId--);
        (endId > nodeId) && (endId--);
        currentEdge.attr("id", currentId.slice(0,3) + startId + "-n" + endId);
        dir && currentArrow.attr("id", "an" + startId + "-n" + endId);
        currentEdgeText.attr("id", "tn" + startId + "-n" + endId);
    }
}