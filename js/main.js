/**
 * Created by Moan on 28/04/16.
 */

var adjacencyMatrix = [];
var edgeDirectedMatrix = [];

var graphArea = new DrawingArea("graph-drawing");


function addNewNodeToMatrix() {
    if (adjacencyMatrix.length == 0) {
        adjacencyMatrix.push([0]);
        edgeDirectedMatrix.push([false]);
    } else {
        adjacencyMatrix.forEach(function(list) {
            list.push(0);
        });
        edgeDirectedMatrix.forEach(function(list) {
            list.push(false);
        })
        adjacencyMatrix.push(Array(adjacencyMatrix[0].length).fill(0));
        edgeDirectedMatrix.push(Array(edgeDirectedMatrix[0].length).fill(false));
    }
}

function removeNodeFromMatrix(node) {
    for(var i = 0; i < adjacencyMatrix.length; i++) {
        adjacencyMatrix[i] = adjacencyMatrix[i].slice(0,node).concat(adjacencyMatrix[i].slice(node+1));
        edgeDirectedMatrix[i] = edgeDirectedMatrix[i].slice(0,node).concat(edgeDirectedMatrix[i].slice(node+1));
    }
    adjacencyMatrix = adjacencyMatrix.slice(0,node).concat(adjacencyMatrix.slice(node+1));
    edgeDirectedMatrix = edgeDirectedMatrix.slice(0,node).concat(edgeDirectedMatrix.slice(node+1));
}

function addEdgeToMatrix(start, end, weight) {
    adjacencyMatrix[start][end] = weight;
}

function setDirected(start, end) {
    edgeDirectedMatrix[start][end] = true;
}

function checkIfUndirected(start, end) {
    return ((adjacencyMatrix[start][end] != 0) && (adjacencyMatrix[end][start] !=0)
                && (!checkIfDirected(start, end) || !checkIfDirected(end, start)));
}

function checkIfDirected(start,end) {
    return edgeDirectedMatrix[start][end];
}

function getEdgeWeight(start,end) {
    return adjacencyMatrix[start][end];
}

function removeEdgeFromMatrix(startNode, endNode) {
    adjacencyMatrix[startNode][endNode] = 0;
    edgeDirectedMatrix[startNode][endNode] = false;
}