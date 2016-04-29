/**
 * Created by Moan on 28/04/16.
 */


var width;
var height = 400;
var radius = 15;
var themeColor = "magenta";

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

    // Handler for the "Add node" button
    $("#addNodeButton").click(function() { addNode(vis); });

    // Handler for the "Add edge" button
    $("#addEdgeButton").click(function() {addEdge(vis); });
};


// Function for adding a node to the middle of the drawing area
function addNode(vis) {
    var node = vis.svg.append("g")
        .attr("height", 30)
        .attr("width", 30)
        .attr("transform", "translate(" + vis.centerX + "," + vis.centerY + ")");

    node.append("text")
        .attr("text-anchor", "middle")
        .attr("dy", ".35em")
        .attr("font-color", themeColor)
        .text(vis.nodeCounter);

    var drag = d3.behavior.drag()
        .on("drag", dragmove)
        .on("dragstart", startDrag)
        .on("dragend", endDrag);


    node.append("circle")
        .attr("r", radius)
        .attr("cx", 0)
        .attr("cy", 0)
        .attr("id", "n" + vis.nodeCounter)
        .attr("fill", themeColor)
        .attr("stroke", themeColor)

        .attr("fill-opacity", 0)
        .call(drag);

    vis.nodeCounter++;


};

// Function for adding an edge between two already drawn nodes
function addEdge(vis) {
    var endNode = parseInt($("#endNode").val());
    var startNode = parseInt($("#startNode").val());

    var coordinates = {};
    coordinates.x1 = parseFloat(d3.select("#n" + startNode).attr("cx"));
    coordinates.y1 = parseFloat(d3.select("#n" + startNode).attr("cy"));
    coordinates.x2 = parseFloat(d3.select("#n" + endNode).attr("cx"));
    coordinates.y2 = parseFloat(d3.select("#n" + endNode).attr("cy"));
    var calculatedCoordinates = calcXY(coordinates);
    console.log(calculatedCoordinates);

    vis.g.append("line")
        .attr("x1", calculatedCoordinates.x1 + coordinates.x1)
        .attr("y1", calculatedCoordinates.y1 + coordinates.y1)
        .attr("x2", calculatedCoordinates.x2 + coordinates.x2)
        .attr("y2", calculatedCoordinates.y2 + coordinates.y2)
        .attr("id", "ln" + startNode + "-n" + endNode)
        .attr("stroke-width", "2px")
        .attr("stroke", themeColor);


};

function calcXY(coordinates) {
    var x1 = coordinates.x1;
    var y1 = coordinates.y1;
    var x2 = coordinates.x2;
    var y2 = coordinates.y2;
    var result = {};

    var dist = Math.sqrt(Math.pow(x1-x2,2) + Math.pow(y1-y2,2));
    var µ1 = Math.atan2((y2-y1) , (x2-x1));
    var µ2 = Math.atan2((y1-y2) , (x1-x2));
    result.x1 = radius * Math.cos(µ1);
    result.y1 = radius * Math.sin(µ1);
    result.x2 = radius * Math.cos(µ2);
    result.y2 = radius * Math.sin(µ2);
    return result;
}

function dragmove() {
    var circle = d3.select(this);
    circle
        .attr("cx", Math.max(Math.min(d3.event.x, (width/2.0) - radius - 7), -(width/2.0) + radius + 1))
        .attr("cy", Math.max(Math.min(d3.event.y, (height/2.0) - radius - 7), -(height/2.0) + radius + 1));


    

    var start = "[id^=l" + circle.attr("id") + "-]";
    var end = "[id$=-" + circle.attr("id") + "]";
    d3.selectAll(start)
        .attr("x1", circle.attr("cx"))
        .attr("y1", circle.attr("cy"));

    d3.selectAll(end)
        .attr("x2", circle.attr("cx"))
        .attr("y2", circle.attr("cy"));


}

function startDrag() {
    d3.select(this.parentElement).select("text").attr("visibility", "hidden");
    d3.select(this).attr("fill-opacity", 1);

}

function endDrag() {
    d3.select(this).attr("fill-opacity", 0);
    d3.select(this.parentElement).select("text")
        .attr("dx", d3.select(this).attr("cx"))
        .attr("dy", parseFloat(d3.select(this).attr("cy")) + 5)
        .attr("visibility", "visible");
}