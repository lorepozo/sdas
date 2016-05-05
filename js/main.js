/**
 * Created by Moan on 28/04/16.
 */

var adjacencyMatrix = [];
var edgeDirectedMatrix = [];
var graphArea = new DrawingArea("graph-drawing");
var playing = false;

// Mode changing
$("#playModeButtons").hide();
$("#playModeButton").click(function() {
    $("#playModeButtons").show();
    $("#addModeButtons").hide();
    step = {"textNode" : [], "textEdge" : [], "highlightNode" : [], "highlightEdge" : []};
    stepHistory = [step];
    compute();
});

$("#deleteModeButton").click(function() {
    $("#addModeButtons").hide();
    $("#playModeButtons").hide();
    addMode = false;
    reset(reverseHistory);
    currentStep = 0;
    stepHistory = [];
    reverseHistory = [];
    stepNumber = 0;
});

$("#addModeButton").click(function() {
    $("#addModeButtons").show();
    $("#playModeButtons").hide();
    addMode = true;
    reset(reverseHistory);
    currentStep = 0;
    stepHistory = [];
    reverseHistory = [];
    stepNumber = 0;
});


// Mechanisms for play-pause, backwards and forward
$("#play-pause").click(function() {
    $(this).find(">:first-child").attr("class", function() {
        if(playing) {
            return "fa fa-play";
        } return "fa fa-pause";
    });
    playing = !playing;
    playing && playForward(stepHistory);
});

$("#backward").click(function() {
    $("#play-pause").find(">:first-child").attr("class", "fa fa-play");
    playing = false;
    playBackward(reverseHistory);
});

$("#forward").click(function() {
    $("#play-pause").find(">:first-child").attr("class", "fa fa-play");
    playing = false;
    playOneStepForward(stepHistory);
});

// Handler for the "Add node" button
$("#addNodeButton").click(function() { addNode(graphArea); });



// Handler for the "Add edge" button
$("#addEdgeButton").click(function() {addEdge(graphArea, false);});
$("#addDirEdgeButton").click(function() {addEdge(graphArea, true); });

// Remove value in text fields when clicked
$("body").click(function() {
        $("#startNode").val("Start");
        $("#endNode").val("End");
        $("#nodeText").val("Node text");
        $("#weight").val("Weight")})
    .find("#weight")
    .on("click", function(e) {
        e.stopPropagation();
    })
    .parent().find("#startNode")
    .on("click", function(e) {
        e.stopPropagation();
    })
    .parent().find("#endNode")
    .on("click", function(e) {
        e.stopPropagation();
    })
    .parent().parent().find("#nodeText")
    .on("click", function(e) {
        e.stopPropagation();
    });

$("#startNode").click(function() {$(this).val("")});
$("#endNode").click(function() {$(this).val("")});
$("#weight").click(function() {$(this).val("")});
$("#nodeText").click(function () {$(this).val("")});
$("#noRounds").click(function () {$(this).val("")});


