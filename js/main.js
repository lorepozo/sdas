/* TODO: clean code, sizing of edges in delete mode, delete from local storage*/

var adjacencyMatrix = [];
var edgeDirectedMatrix = [];
var playing = false;
var maximized = false;
var endReached = false;
var addMode = true;
var autoGenerateRandomizer = 0;

var algorithm;
var interpreter = new BiwaScheme.Interpreter(function(e) {
    console.error(e.message);
});
var stepNumber = -1;

var editor = addEditor();
var graphArea = new DrawingArea("graph-drawing");

$(".save").hide();
$(".playModeButtons").hide();
$("#clearGraphButton").hide();
$("#addModeButton").click();

// start with a graph already generated
generateMSTGraph(graphArea);

// initialize with leader elect
$.get('scheme/algs/leader_elect.scm', function(data) {
    editor.getDoc().setValue(';;; Scheme code editor ;;;\n'+data);
});

function reset() {
    stepToInitialState();
    currentStep = 0;
    $("#stepCounter").text(0);
    stepHistory = [];
    stepNumber = 0;
    endReached = false;
    playing = false;
}

function play() {
    setInitialState();
    stepHistory = [];
    currentStep = 0;
    stepNumber = -1;

    algorithm = new Algorithm({
        code: editor.getDoc().getValue(),
        rounds: $("#noRounds").val(),
        start: $("#algorithmStartNode").val(),
    });

    algorithm.compute();
}

