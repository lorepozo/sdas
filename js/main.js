/**
 * Created by Moan on 28/04/16.
 */

var adjacencyMatrix = [];
var edgeDirectedMatrix = [];
var graphArea = new DrawingArea("graph-drawing");
var playing = false;
var maximized = false;

var autoGenerateRandomizer = 0;

function clearGraph() {
    $('#graph-svg').remove();
    adjacencyMatrix = [];
    edgeDirectedMatrix = [];
    graphArea = new DrawingArea("graph-drawing");
}

$("#autoGenerate").click(function() {
    clearGraph();
    switch (autoGenerateRandomizer++) {
    case 0:  generateBellmanFordGraph(graphArea); break;
    case 1:  generateLeaderElectGraph(graphArea); break;
    case 2:  generateLubyMISGraph(graphArea); break;
    default: generateMSTGraph(graphArea);
             autoGenerateRandomizer = 0;
    }
});


// Mode changing
$("#save").hide();
$("#playModeButtons").hide();
$("#playModeButton").click(function() {
    $("#playModeButtons").show();
    $('#stepCounter').show();
    $("#addModeButtons").hide();
    $("#addModeButton").removeClass("active");
    $("#addModeButton").removeClass("active");
    play();
});

function play() {
    setInitialState();
    stepHistory = [];
    currentStep = 0;
    stepNumber = -1;

    algorithmStartNode = $("#algorithmStartNode").val();
    if (!isInt(algorithmStartNode)) algorithmStartNode = 0;

    algorithmNumberOfRounds = $("#noRounds").val();
    !isInt(algorithmNumberOfRounds) && (algorithmNumberOfRounds = 15);

    algorithm = editor.getDoc().getValue();
    compute();
}

$("#deleteModeButton").click(function() {
    $("#addModeButtons").hide();
    $("#playModeButtons").hide();
    $('#stepCounter').hide();
    $(this).addClass("active");
    $("#addModeButton").removeClass("active");
    addMode = false;
    playing = false;
    stepToInitialState();
    currentStep = 0;
    stepHistory = [];
    stepNumber = 0;
});

$("#addModeButton").click(function() {
    $("#addModeButtons").show();
    $("#playModeButtons").hide();
    $('#stepCounter').hide();
    $(this).addClass("active");
    $("#deleteModeButton").removeClass("active");
    addMode = true;
    playing = false;
    stepToInitialState();
    currentStep = 0;
    stepHistory = [];
    stepNumber = 0;
});


$("#minimizeEditor").click(function() {
    if (maximized) {
        $("#save").show();
        $("#textEditorBorder").css("visibility", "hidden");
        editor.getWrapperElement().style.display = "none";
        $(this).text("Show code editor");
        maximized = !maximized;
    } else {
        $("#save").hide();
        $("#textEditorBorder").css("visibility", "visible");
        editor.getWrapperElement().style.display = "inherit";
        $(this).text("Minimize editor");
        maximized = !maximized;
    }
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
    playBackward(stepHistory);
});

$("#forward").click(function() {
    $("#play-pause").find(">:first-child").attr("class", "fa fa-play");
    playing = false;
    playOneStepForward(stepHistory);
});

// Handler for play mode reset button
$("#playModeResetButton").click(function() {
    //TODO this should be done better
    $("#addModeButton").click();
    $("#playModeButton").click();
    $("#stepCounter").text(0);
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
        $("#weight").val("Weight");
    }).find("#weight")
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
    });

$("#startNode").click(function() {$(this).val("")});
$("#endNode").click(function() {$(this).val("")});
$("#weight").click(function() {$(this).val("")});
$("#nodeText").click(function () {$(this).val("")});
$("#noRounds").click(function () {$(this).val("")});
$("#algorithmStartNode").click(function () {$(this).val("")});

$("#endNode").keydown(function(e) {if (e.keyCode == 13) $("#addEdgeButton").click()});
$("#weight").keydown(function(e) {if (e.keyCode == 13) $("#addDirEdgeButton").click()});
$("#algorithmStartNode").keydown(function (e) {if (e.keyCode == 13) $("#playModeButton").click()});
$("#noRounds").keydown(function (e) {if (e.keyCode == 13) $("#playModeButton").click()});

// start with a graph already generated
clearGraph();
generateMSTGraph(graphArea);
