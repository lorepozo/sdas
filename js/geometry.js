/**
 * Created by Moan on 02/05/16.
 */

function calcXY(startNode, endNode) {
    var x1 = parseFloat(d3.select("#n" + startNode).attr("cx"));
    var y1 = parseFloat(d3.select("#n" + startNode).attr("cy"));
    var x2 = parseFloat(d3.select("#n" + endNode).attr("cx"));
    var y2 = parseFloat(d3.select("#n" + endNode).attr("cy"));
    var result = {};

    var dist = Math.sqrt(Math.pow(x1-x2,2) + Math.pow(y1-y2,2));
    var µ1 = Math.atan2((y2-y1) , (x2-x1));
    var µ2 = Math.atan2((y1-y2) , (x1-x2));
    result.x1 = radius * Math.cos(µ1) + x1;
    result.y1 = radius * Math.sin(µ1) + y1;
    result.x2 = radius * Math.cos(µ2) + x2;
    result.y2 = radius * Math.sin(µ2) + y2;
    result.µ1 = µ1;
    result.µ2 = µ2;
    return result;
}


function calcAnglePoints(coordinates) {
    var x1 = coordinates.x1;
    var y1 = coordinates.y1;
    var x2 = coordinates.x2;
    var y2 = coordinates.y2;
    var µ1 = coordinates.µ1;
    var µ2 = coordinates.µ2;

    var result = {};

    result.x1 = x1 - curveWidth * Math.sin(µ1);
    result.y1 = y1 + curveWidth * Math.cos(µ1);
    result.x2 = x2 + curveWidth * Math.sin(µ2);
    result.y2 = y2 - curveWidth * Math.cos(µ2);
    result.µ1 = µ1;
    result.µ2 = µ2;

    return "M " + x1 + " " + y1 + " C " + result.x1 + " " + result.y1 + " " + result.x2 + " " + result.y2 + " " + x2 + " " + y2;
}


function calcArrowPosition(coordinates, dist) {
    var x1 = coordinates.x1;
    var y1 = coordinates.y1;
    var x2 = coordinates.x2;
    var y2 = coordinates.y2;
    var µ1 = coordinates.µ1;
    var µ2 = coordinates.µ2;

    var center = Math.sqrt(Math.pow(x2-x1,2) + Math.pow(y2-y1,2)) / 2.0;

    var x = ((x1 <= x2) ? center * Math.cos(µ1) + x1 : center * Math.cos(µ2) + x2);
    var y = ((y1 <= y2) ? center * Math.sin(µ1) + y1 : center * Math.sin(µ2) + y2);

    x -= dist*Math.sin(µ1);
    y += dist*Math.cos(µ1);
    var angle = 90 + 180 / Math.PI * µ1;

    return "translate(" + x + "," + y + "), rotate(" + angle + ")"
}


function calcEdgeTextPosition(coordinates, dist) {
    var x1 = coordinates.x1;
    var y1 = coordinates.y1;
    var x2 = coordinates.x2;
    var y2 = coordinates.y2;
    var µ1 = coordinates.µ1;
    var µ2 = coordinates.µ2;

    var center = Math.sqrt(Math.pow(x2-x1,2) + Math.pow(y2-y1,2)) / 2.0;

    var x = ((x1 <= x2) ? center * Math.cos(µ1) + x1 : center * Math.cos(µ2) + x2);
    var y = ((y1 <= y2) ? center * Math.sin(µ1) + y1 : center * Math.sin(µ2) + y2);
    x -= dist*Math.sin(µ1);
    y += dist*Math.cos(µ1);

    return "translate(" + x + "," + y + ")";
}


function calcNextXY(r,angle) {
    var x = r*Math.cos(angle);
    var y = r*Math.sin(angle);
    angle += 160 * Math.PI / 180.0;
    return [x,y,angle];
}


