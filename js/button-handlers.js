/**
 * Created by Moan on 09/05/16.
 */

$("#autoGenerate").click(function() {
    graphArea.removeAllNodes();
    switch (autoGenerateRandomizer++) {
        case 0:  generateBellmanFordGraph(graphArea); break;
        case 1:  generateLeaderElectGraph(graphArea); break;
        case 2:  generateLubyMISGraph(graphArea); break;
        default: generateMSTGraph(graphArea);
            autoGenerateRandomizer = 0;
    }
});


// Handling mode switching
$("#deleteModeButton").click(function() {
    $("#addModeButtons").hide();
    $('#stepCounter').hide();
    $(this).css("background-color", "white").css("color", "black");
    $("#addModeButton").css("background-color", "black").css("color", "white");
    $("#clearGraphButton").show();
    addMode = false;

    // Resetting all algorithm related variables
    stepToInitialState();
    currentStep = 0;
    $("#stepCounter").text(0);
    stepHistory = [];
    stepNumber = 0;
    endReached = false;
    playing = false;
});

$("#addModeButton").click(function() {
    $("#addModeButtons").show();
    $(".playModeButtons").hide();
    $('#stepCounter').hide();
    $("#clearGraphButton").hide();
    $(this).css("background-color", "white").css("color", "black");
    $("#deleteModeButton").css("background-color", "black").css("color", "white");
    addMode = true;

    // Resetting all algorithm related variables
    stepToInitialState();
    currentStep = 0;
    $("#stepCounter").text(0);
    stepHistory = [];
    stepNumber = 0;
    endReached = false;
    playing = false;
});

$("#playModeButton").click(function() {
    $(".playModeButtons").show();
    $('#stepCounter').show();
    $("#addModeButtons").hide();
    $("#clearGraphButton").hide();
    $("#deleteModeButton").css("background-color", "black").css("color", "white");
    $("#addModeButton").css("background-color", "black").css("color", "white");
    addMode = true;
    play();
});


// Minimize/Maximize the code editor
$("#minimizeEditor").click(function() {
    if (maximized) {
        $(".save").hide();
        $("#textEditorBorder").css("visibility", "hidden");
        editor.getWrapperElement().style.display = "none";
        $(this).text("Show code editor");
        maximized = !maximized;
    } else {
        $(".save").show();
        $("#textEditorBorder").css("visibility", "visible");
        editor.getWrapperElement().style.display = "inherit";
        $(this).text("Minimize editor");
        maximized = !maximized;
    }
});

// Saving and deleting of algorithms
$("#save").click(function() {
    save();
});

$("#saveAs").click(function () {
    saveAs();
});


// Mechanisms for play-pause, backwards and forward
$("#play-pause").click(function() {
    if(playing) {
        $(this).find(">:first-child").attr("class", "fa fa-play");
        playing = false;
    } else {
        if (!endReached) {
            $(this).find(">:first-child").attr("class", "fa fa-pause");
            playForward(stepHistory);
            playing = true;
        }
        return;
    }
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
    playing = false;
    endReached = false;
    stepToInitialState();
    play();
    $("#stepCounter").text(0);
});


// Handler for "Clear graph" button
$("#clearGraphButton").click(function() {
    graphArea.removeAllNodes();
});

// Handler for the "Add node" button
$("#addNodeButton").click(function() { addNode(graphArea); });

// Handler for the "Add edge" button
$("#addEdgeButton").click(function() {addEdge(graphArea, false);});
$("#addDirEdgeButton").click(function() {addEdge(graphArea, true); });



$("#endNode").keydown(function(e) {if (e.keyCode == 13) $("#addEdgeButton").click()});
$("#weight").keydown(function(e) {if (e.keyCode == 13) $("#addDirEdgeButton").click()});
$("#algorithmStartNode").keydown(function (e) {if (e.keyCode == 13) $("#playModeButton").click()});
$("#noRounds").keydown(function (e) {if (e.keyCode == 13) $("#playModeButton").click()});
