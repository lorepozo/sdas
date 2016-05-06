/**
 * Created by Moan on 06/05/16.
 */


function generateLeaderElectGraph(vis) {
    var coordinates = [[-140,-110],[0,-145],[-140,60],[0,0],[145,-10],[-10, 120]];
    coordinates.forEach(function(coordinate) {
        var node = vis.svg.append("g")
            .attr("height", 30)
            .attr("width", 30)
            .attr("transform", "translate(" + vis.centerX + "," + vis.centerY + ")");

        node.append("text")
            .attr("text-anchor", "middle")
            .attr("dx", coordinate[0])
            .attr("dy", coordinate[1] + 5)
            .attr("font-color", themeColor)
            .attr("id", "tn" + vis.nodeCounter)
            .text(vis.nodeCounter);

        node.append("text")
            .attr("text-anchor", "middle")
            .attr("dy", coordinate[1]-17.5)
            .attr("dx", coordinate[0]-15)
            .attr("id", "nodeText" + vis.nodeCounter);

        var drag = d3.behavior.drag()
            .on("drag", dragmove)
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
            })
            .call(drag);

        vis.nodeCounter++;
        addNewNodeToMatrix();
    });

    var edgeCoordinates = [[1,0, calcXY(1, 0), false],
        [0,2, calcXY(0, 2), true],
        [2,0, calcXY(2, 0), true],
        [0,3, calcXY(0, 3), false],
        [5,2, calcXY(5, 2), false],
        [5,3, calcXY(5, 3), false],
        [1,3, calcXY(1, 3), false],
        [3,4, calcXY(3, 4), false],
        [4,1, calcXY(4, 1), true],
        [1,4, calcXY(1, 4), true],
        [4,5, calcXY(4, 5), false]];
    
    edgeCoordinates.forEach(function(edge) {
        addDirectedEdge(vis, edge[0], edge[1], edge[2], 0, edge[3]);
    })

}