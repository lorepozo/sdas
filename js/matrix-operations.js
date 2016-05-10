/**
 * Created by Moan on 04/05/16.
 */

function addNewNodeToMatrix() {
    if (adjacencyMatrix.length == 0) {
        adjacencyMatrix.push([[]]);
        edgeDirectedMatrix.push([false]);
    } else {
        adjacencyMatrix.forEach(function(list) {
            list.push([]);
        });
        edgeDirectedMatrix.forEach(function(list) {
            list.push(false);
        });
        adjacencyMatrix.push(Array(adjacencyMatrix[0].length).fill([]));
        edgeDirectedMatrix.push(Array(edgeDirectedMatrix[0].length).fill(false));
    }
}

function removeNodeFromMatrix(node) {
    node = parseInt(node);
    for(var i = 0; i < adjacencyMatrix.length; i++) {
        var front = adjacencyMatrix[i].slice(0,node);
        var back = adjacencyMatrix[i].slice(node+1);

        adjacencyMatrix[i] = front.concat(back);
        edgeDirectedMatrix[i] = edgeDirectedMatrix[i].slice(0,node).concat(edgeDirectedMatrix[i].slice(node+1));
    }
    adjacencyMatrix = adjacencyMatrix.slice(0,node).concat(adjacencyMatrix.slice(node+1));
    edgeDirectedMatrix = edgeDirectedMatrix.slice(0,node).concat(edgeDirectedMatrix.slice(node+1));
}

function addEdgeToMatrix(start, end, weight) {
    adjacencyMatrix[start][end] = isNaN(weight) ? 0 : weight;
}

function setDirected(start, end) {
    edgeDirectedMatrix[start][end] = true;
}

function checkIfUndirected(start, end) {
    return ((!Array.isArray(adjacencyMatrix[start][end])) && (!Array.isArray(adjacencyMatrix[end][start]))
    && (!checkIfDirected(start, end) || !checkIfDirected(end, start)));
}

function checkIfDirected(start,end) {
    return edgeDirectedMatrix[start][end];
}

function getEdgeWeight(start,end) {
    return adjacencyMatrix[start][end];
}

function removeEdgeFromMatrix(startNode, endNode) {
    adjacencyMatrix[startNode][endNode] = [];
    edgeDirectedMatrix[startNode][endNode] = false;
}

function undefinedToList(value){
    return (value == null ? [] : value);
}
