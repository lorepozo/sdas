/* Geometry.js contains functions calculating the positioning of edges and nodes */

/**
 * @param start : Integer representing start node
 * @param end : Integer representing end node
 * @returns result : JS object containing 4 coordinates (representing 2 points) and 2 angle values (radians)
 */
function calcXY(start, end) {
  var coords = getCircleCoordinates(start, end);
  var result = {};

  var µ1 = Math.atan2((coords.y2 - coords.y1), (coords.x2 - coords.x1));
  var µ2 = Math.atan2((coords.y1 - coords.y2), (coords.x1 - coords.x2));
  result.x1 = radius * Math.cos(µ1) + coords.x1;
  result.y1 = radius * Math.sin(µ1) + coords.y1;
  result.x2 = radius * Math.cos(µ2) + coords.x2;
  result.y2 = radius * Math.sin(µ2) + coords.y2;
  result.µ1 = µ1;
  result.µ2 = µ2;

  return result;
}


/**
 * @param coord : object returned from call to calcXY
 * @returns {string} : for setting a curved path from a node to another
 */
function calcAnglePoints(coord, curveWidth) {
  var x1 = coord.x1 - curveWidth * Math.sin(coord.µ1);
  var y1 = coord.y1 + curveWidth * Math.cos(coord.µ1);
  var x2 = coord.x2 + curveWidth * Math.sin(coord.µ2);
  var y2 = coord.y2 - curveWidth * Math.cos(coord.µ2);
  return "M " + coord.x1 + " " + coord.y1 + " C " + x1 + " " + y1 + " " + x2 + " " + y2 + " " + coord.x2 + " " + coord.y2;
}


/**
 *
 * @param coord : object returned from call to calcXY
 * @returns {*[]} : list containing the coordinates of the center between two nodes
 */
function findCenter(coord) {
  var center = Math.sqrt(Math.pow(coord.x2 - coord.x1, 2) + Math.pow(coord.y2 - coord.y1, 2)) / 2.0;
  var x = ((coord.x1 <= coord.x2) ? center * Math.cos(coord.µ1) + coord.x1 : center * Math.cos(coord.µ2) + coord.x2);
  var y = ((coord.y1 <= coord.y2) ? center * Math.sin(coord.µ1) + coord.y1 : center * Math.sin(coord.µ2) + coord.y2);
  return [x, y];
}


/**
 * Calculates the path of a self-edge
 * @param node
 * @returns {string}
 */
function calcSelfEdgeCoords(node) {
  var x1 = parseFloat(d3.select("#n" + node).attr("cx"));
  var y1 = parseFloat(d3.select("#n" + node).attr("cy"));
  var startX = x1 + radius * Math.cos(5 * Math.PI / 6);
  var startY = y1 - radius * Math.sin(5 * Math.PI / 6);
  var endX = x1 + radius * Math.cos(Math.PI / 6);
  var endY = y1 - radius * Math.sin(Math.PI / 6);

  var angleY1 = startY - 50;
  var angleY2 = endY - 50;

  return "M " + startX + " " + startY + " C" + startX + " " + angleY1 + " " + endX + " " + angleY2 + " " + endX + " " + endY;
}


/**
 * Calculates text position of text associated with self-edges
 * @param node
 * @param dist
 * @returns {string}
 */
function calcSelfEdgeTextPosition(node, dist) {
  var x1 = parseFloat(d3.select("#n" + node).attr("cx"));
  var y1 = parseFloat(d3.select("#n" + node).attr("cy"));
  return "translate(" + x1 + "," + (y1 - dist) + ")";
}


/**
 * @param coord : object returned from call to calcXY
 * @param dist : pixel value used for offsetting
 * @returns {string} : for setting the position and angle of an arrow on an edge
 */
function calcArrowPosition(coord, dist) {
  var angle = 90 + 180 / Math.PI * coord.µ1;
  return calcEdgeTextPosition(coord, dist) + ", rotate(" + angle + ")";
}

/**
 * @param coord : object returned from call to calcXY
 * @param dist : pixel value used for offsetting
 * @returns {string} : for setting the position of text on an edge
 */
function calcEdgeTextPosition(coord, dist) {
  var center = findCenter(coord);
  var x = center[0] - dist * Math.sin(coord.µ1);
  var y = center[1] + dist * Math.cos(coord.µ1);
  return "translate(" + x + "," + y + ")";
}


/**
 * Calculates the position of the edge weight text
 * @param coord : object returned from call to calcXY
 * @param dist : float value used for offsetting
 * @returns {string} : for setting the position of static text on an edge
 */
function calcStaticEdgeTextPosition(coord, dist) {
  var center = findCenter(coord);
  var x = center[0] + dist * Math.sin(coord.µ1);
  var y = center[1] - dist * Math.cos(coord.µ1);
  return "translate(" + x + "," + y + ")";
}


/**
 * @param r : radius in pixels
 * @returns {*[]} : position relative to center of graph drawing area
 */
function calcNextXY(r) {
  var x = r * Math.cos(angle);
  var y = r * Math.sin(angle);
  angle += 160 * Math.PI / 180.0;
  return [x, y];
}


/**
 * Input:  Two integers representing nodes
 * Output: JS Object containing the coordinates of the given nodes
 */
function getCircleCoordinates(startNode, endNode) {
  var result = {};
  result.x1 = parseFloat(d3.select("#n" + startNode).attr("cx"));
  result.y1 = parseFloat(d3.select("#n" + startNode).attr("cy"));
  result.x2 = parseFloat(d3.select("#n" + endNode).attr("cx"));
  result.y2 = parseFloat(d3.select("#n" + endNode).attr("cy"));

  return result;
}

