/**
 * Created by Moan on 06/05/16.
 */

function genAddCoordinates(vis) {
    return function(coordinate) {
        //scale coord from 400-scale to height-scale
        coordinate = [coordinate[0]*vis.height/400,
                      coordinate[1]*vis.height/400];

        //add node
        var node = vis.svg.append("g")
            .attr("height", 30)
            .attr("width", 30)
            .attr("transform", "translate(" + vis.centerX + "," + vis.centerY + ")");

        node.append("text")
            .attr("text-anchor", "middle")
            .attr("dx", coordinate[0])
            .attr("dy", coordinate[1] + 5)
            .attr("fill", themeColor)
            .attr("font-size", themeFontSize)
            .attr("font-weight", themeFontWeight)
            .attr("id", "tn" + vis.nodeCounter)
            .text(vis.nodeCounter);

        node.append("text")
            .attr("text-anchor", "middle")
            .attr("dy", coordinate[1]-radius)
            .attr("dx", coordinate[0]-radius)
            .attr("fill", themeColor)
            .attr("font-size", themeChangedFontSize)
            .attr("font-weight", themeFontWeight)
            .attr("id", "nodeText" + vis.nodeCounter);

        var drag = d3.behavior.drag().on("drag", dragmove)
            .on("dragstart", startDrag)
            .on("dragend", endDrag);

        node.append("circle")
            .attr("r", radius)
            .attr("cx", coordinate[0])
            .attr("cy", coordinate[1])
            .attr("id", "n" + vis.nodeCounter)
            .attr("fill", themeColor)
            .attr("stroke", themeColor)
            .attr("fill-opacity", 0)
            .on("click", function() {
                if (!addMode) {
                    var id = $(this).attr("id");
                    removeAllEdges(id);
                    $(this).parent().remove();
                    $("#t" + id).remove();
                    removeNodeFromMatrix(id.slice(1));
                    changeNodeNumbering(id.slice(1));
                    vis.nodeCounter--;
                }
            }).call(drag);

        vis.nodeCounter++;
        addNewNodeToMatrix();
    }
}

function genAddUndirectedEdges(vis) {
    return function(edge) {
        addUndirectedEdge(vis, edge[0], edge[1], calcXY(edge[0], edge[1]), edge[2])
    }
}

function genAddDirectedEdges(vis) {
    return function(edge) {
        addDirectedEdge(vis, edge[0], edge[1], calcXY(edge[0], edge[1]), edge[2], edge[3]);
    }
}

function generateBellmanFordGraph(vis) {
    var coordinates = [[-140,-110],[0,-145],[-140,60],[0,0],[145,-10],[-10, 120]];
    coordinates.forEach(genAddCoordinates(vis));

    var edgeCoordinates = [
        [0,2,8, true ],
        [1,0,7, false],
        [1,3,5, false],
        [1,4,10,true ],
        [2,0,6, true ],
        [3,0,11,false],
        [3,4,2, false],
        [4,1,1, true ],
        [4,5,4, false],
        [5,2,3, false],
        [5,3,9, false]];
    edgeCoordinates.forEach(genAddDirectedEdges(vis))
}

function generateLeaderElectGraph(vis) {
    var coordinates = [[-140,-110],[0,-145],[-140,60],[0,0],[145,-10],[-10, 120]];
    coordinates.forEach(genAddCoordinates(vis));

    var edgeCoordinates = [
        [0,2,NaN,true ],
        [1,0,NaN,false],
        [1,3,NaN,false],
        [1,4,NaN,true ],
        [2,0,NaN,true ],
        [3,0,NaN,false],
        [3,4,NaN,false],
        [4,1,NaN,true ],
        [4,5,NaN,false],
        [5,2,NaN,false],
        [5,3,NaN,false]];
    edgeCoordinates.forEach(genAddDirectedEdges(vis))
}

function generateLubyMISGraph(vis) {
    var coordinates = [
        [-200, 0],
        [-150, 100],
        [-100, 25],
        [-20,  120],
        [115,  130],
        [70,   50],
        [10,   -50],
        [180,  15],
        [190,  -110],
        [80,   -125]];
    coordinates.forEach(genAddCoordinates(vis));

    var edgeCoordinates = [
        [0,2,NaN],
        [1,2,NaN],
        [1,3,NaN],
        [2,3,NaN],
        [2,5,NaN],
        [2,6,NaN],
        [3,4,NaN],
        [3,5,NaN],
        [3,6,NaN],
        [4,5,NaN],
        [4,7,NaN],
        [5,6,NaN],
        [5,7,NaN],
        [6,7,NaN],
        [6,9,NaN],
        [7,8,NaN],
        [8,9,NaN]];
    //       Batman!
    edgeCoordinates.forEach(genAddUndirectedEdges(vis))
}

function generateMSTGraph(vis) {
    var coordinates = [
        [-200, -130],
        [-220,   30],
        [-140,  -20],
        [ -80, -110],
        [-100,   60],
        [  20,    0],
        [  60, -160],
        [  50,  160],
        [ 120,  -60],
        [ 180,  120],
        [ 220,   20]];
    coordinates.forEach(genAddCoordinates(vis));

    var edgeCoordinates = [
        [0,2, 12],
        [1,2, 0],
        [2,3, 5],
        [2,4, 8],
        [3,4, 1],
        [3,5, 2],
        [3,6, 4],
        [4,5, 3],
        [4,7, 6],
        [5,8, 9],
        [7,9, 11],
        [8,9, 7],
        [8,10,10],
        [9,10,13]];
    edgeCoordinates.forEach(genAddUndirectedEdges(vis))
}
