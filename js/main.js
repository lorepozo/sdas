/* TODO: clean code, sizing of edges in delete mode, delete from local storage*/

var adjacencyMatrix = [];
var edgeDirectedMatrix = [];
var playing = false;
var maximized = false;
var endReached = false;
var addMode = true;
var autoGenerateRandomizer = 0;

var graphArea = new DrawingArea("graph-drawing");
addEditor();
addDefaultDropdownOptions();
addAlgorithmsFromStorage();
$(".save").hide();
$(".playModeButtons").hide();
$("#clearGraphButton").hide();
$("#addModeButton").click();



// start with a graph already generated
generateMSTGraph(graphArea);


function play() {
    setInitialState();
    stepHistory = [];
    currentStep = 0;
    stepNumber = -1;

    algorithmStartNode = $("#algorithmStartNode").val();
    !isInt(algorithmStartNode) && (algorithmStartNode = 0);

    algorithmNumberOfRounds = $("#noRounds").val();
    !isInt(algorithmNumberOfRounds) && (algorithmNumberOfRounds = 15);

    algorithm = editor.getDoc().getValue();
    compute();
}



