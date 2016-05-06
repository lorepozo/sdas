/**
 * Created by Moan on 04/05/16.
 */

var step = {"textNode" : [], "textEdge" : [], "highlightNode" : [], "highlightEdge" : []};
var stepHistory;
var currentStep = 0;
var autoplayDelay = 2000;
var initialState;


/*
var testStepHistory1 = [];
var testStepHistory2 = [];

var step1 = {
    "textNode":[{"node":1, "text":"test1"}, {"node":0, "text":"test2"}],
    "highlightNode": [{"node":1, "color":"blue"}, {"node":0, "color":"blue"}],
    "textEdge": [{"start": 0, "end": 1, "text": "0-1"}, {"start": 1, "end": 0, "text": "1-0"}],
    "highlightEdge": [{"start": 0, "end": 1, "color": "green"}, {"start": 1, "end": 0, "color": "green"}]};

var step2 = {
    "textNode":[{"node":2, "text":"test3"}, {"node":3, "text":"test4"}],
    "highlightNode": [{"node":2, "color":"blue"}, {"node":3, "color":"blue"}],
    "textEdge": [{"start": 2, "end": 3, "text": "2-3"}, {"start": 3, "end": 2, "text": "3-2"}],
    "highlightEdge": [{"start": 2, "end": 3, "color": "green"}, {"start": 3, "end": 2, "color": "green"}]};

var step3 = {
    "textNode":[],
    "highlightNode": [],
    "textEdge": [],
    "highlightEdge": [{"start": 0, "end": 1, "color": "red"}, {"start": 0, "end": 2, "color": "red"}]};

var step4 = {
    "textNode":[],
    "highlightNode": [],
    "textEdge": [],
    "highlightEdge": [{"start": 1, "end": 3, "color": "red"}, {"start": 1, "end": 4, "color": "red"},
        {"start": 2, "end": 5, "color": "red"}]};

var step5 = {
    "textNode":[],
    "highlightNode": [],
    "textEdge": [],
    "highlightEdge": [{"start": 3, "end": 6, "color": "red"}, {"start": 5, "end": 7, "color": "red"}]};


testStepHistory1.push(step1);
testStepHistory1.push(step2);
testStepHistory2.push(step3);
testStepHistory2.push(step4);
testStepHistory2.push(step5);
*/

function playForward(stepHistory) {
    var steps = stepHistory.length;
    var playId = setInterval(function(){
        if ((currentStep < steps) && playing) {
            stepForward(stepHistory[currentStep]);
            currentStep++;
        } else {
            clearInterval(playId);
        }
    }, autoplayDelay);
}

function playBackward(stepHistory) {
    if(currentStep == 1) {
        stepToInitialState();
        currentStep--;
    } else if (currentStep > 1) {
        stepForward(stepHistory[currentStep-2]);
        currentStep--;

    }
}

function playOneStepForward(stepHistory) {
    var steps = stepHistory.length;
    if(currentStep < steps) {
        stepForward(stepHistory[currentStep]);
        currentStep++;
    }
}

function stepForward(step) {
    if(currentStep != 0){
        stepToInitialState();
    }

    step.textNode.forEach(function(d) {
        changeNodeText(d.node,d.text);
    });

    step.highlightNode.forEach(function(d) {
        setNodeColor(d.node, d.color);
    });

    step.textEdge.forEach(function(d) {
        changeEdgeText(d.start,d.end,d.text)
    });

    step.highlightEdge.forEach(function(d) {
        setEdgeColor(d.start,d.end,d.color)
    });
}

function stepToInitialState(){
    if(initialState == null) {
        return;
    }
    initialState.textNode.forEach(function(d) {
        changeNodeText(d.node,d.text);
    });

    initialState.highlightNode.forEach(function(d) {
        setNodeColor(d.node, d.color);
    });

    initialState.textEdge.forEach(function(d) {
        changeEdgeText(d.start,d.end,d.text)
    });

    initialState.highlightEdge.forEach(function(d) {
        setEdgeColor(d.start,d.end,d.color)
    });
}

function setInitialState() {
    initialState = {"textNode":[], "highlightNode": [], "textEdge": [], "highlightEdge": []};

    $.each($("[id^=n]circle"), function(i, node) {
        var nodeNumber = node.id.slice(1);
        initialState.textNode.push({"node": nodeNumber, "text": getNodeText(nodeNumber)});
        initialState.highlightNode.push({"node": nodeNumber, "color": getNodeColor(nodeNumber)});
    });

    $.each($("[id^=uln],[id^=dln]"), function(i, edge) {
        var start = edge.id.slice(3, edge.id.indexOf("-"));
        var end = edge.id.slice(edge.id.indexOf("-")+2);
        initialState.textEdge.push({"start": start, "end": end, "text": getEdgeText(start,end)});
        initialState.highlightEdge.push({"start": start, "end": end, "color": getEdgeColor(start, end)});
    })
}

function setNodeColor(node, color) {
    $("#n" + node).css("stroke", color);
}

function setEdgeColor(start, end, color) {
    var idString = "n" + start + "-n" + end;
    var edge, arrow;
    if (!((edge = $("#ul" + idString)).length)) {
        if (!((edge = $("#uln" + end + "-n" + start)).length)) {
            edge = $("#dl" + idString);
            $("#a" + idString).css("fill", color);
        }
    }
    edge.css("stroke", color);
}

function changeEdgeText(start, end, text) {
    var edge;
    if(!((edge = $("#tn" + start + "-n" + end)).length)) {
        $("#tn" + end + "-n" + start).text(text);
    }
    edge.text(text);
}

function changeNodeText(node, text) {
    try {
        text.length;
        $("#nodeText" + node).text(text);
    } catch(e) {
        $("#nodeText" + node).text(" ");
    }
}

function getNodeText(node) {
    return $("#nodeText" + node).text();
}

function getNodeColor(node) {
    return $("#n" + node).css("stroke");
}

function getEdgeText(start,end) {
    var edge;
    if(!((edge = $("#tn" + start + "-n" + end)).length)) {
        return $("#tn" + end + "-n" + start).text();
    }
    return edge.text();
}

function getEdgeColor(start,end) {
    var idString = "n" + start + "-n" + end;
    var edge;
    if(!((edge = $("#ul" + idString)).length)){
        if (!((edge = $("#uln" + end + "-n" + start)).length)) {
            return $("#dl" + idString).css("stroke")
        }
    }
    return edge.css("stroke");
}