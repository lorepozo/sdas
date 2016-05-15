/* button handlers */

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
    $(".playModeButtons").hide();
    $('#stepCounter').hide();
    $("#clearGraphButton").show();
    $(this).css("background-color", "white").css("color", "black");
    $("#addModeButton").css("background-color", "black").css("color", "white");
    addMode = false;

    reset();
});

$("#addModeButton").click(function() {
    $("#addModeButtons").show();
    $(".playModeButtons").hide();
    $('#stepCounter').hide();
    $("#clearGraphButton").hide();
    $(this).css("background-color", "white").css("color", "black");
    $("#deleteModeButton").css("background-color", "black").css("color", "white");
    addMode = true;

    reset();
});

$("#playModeButton").click(function() {
    $("#addModeButtons").hide();
    $(".playModeButtons").show();
    $('#stepCounter').show();
    $("#clearGraphButton").hide();
    $("#deleteModeButton").css("background-color", "black").css("color", "white");
    $("#addModeButton").css("background-color", "black").css("color", "white");
    addMode = true;

    play();
});

$("#playModeResetButton").click(function() {
    reset();
    play();
});


// Minimize/Maximize the code editor
$("#minimizeEditor").click(function() {
    $(".save").toggle();
    $("#textEditorBorder").slideToggle();
    if (maximized) {
        $(this).text("Show code editor");
    } else {
        $(this).text("Minimize editor");
    }
    maximized = !maximized;
});


// Saving and deleting of stored algorithms
$("#save").click(function() {
    save();
});

$("#saveAs").click(function () {
    saveAs();
});


// Mechanisms for play-pause, backwards and forward
$("#play-pause").click(function() {
    if (playing) {
        $(this).find(">:first-child").attr("class", "fa fa-play");
        playing = false;
    } else if (!endReached) {
        $(this).find(">:first-child").attr("class", "fa fa-pause");
        playForward(stepHistory);
        playing = true;
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



// Handler for "Clear graph" button
$("#clearGraphButton").click(function() {
    graphArea.removeAllNodes();
});

// Handler for the "Add node" button
$("#addNodeButton").click(function() { addNode(graphArea); });

// Handler for the "Add edge" button
$("#addEdgeButton").click(function() {addEdge(graphArea, false);});
$("#addDirEdgeButton").click(function() {addEdge(graphArea, true); });


// Handlers for enter key
$("#endNode").keydown(function(e) {if (e.keyCode == 13) $("#addEdgeButton").click()});
$("#weight").keydown(function(e) {if (e.keyCode == 13) $("#addDirEdgeButton").click()});
$("#algorithmStartNode").keydown(function (e) {if (e.keyCode == 13) $("#playModeButton").click()});
$("#noRounds").keydown(function (e) {if (e.keyCode == 13) $("#playModeButton").click()});

