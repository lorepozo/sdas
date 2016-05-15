/**
 * Created by Moan on 04/05/16.
 */

var stepHistory = [];
var currentStep = 0;
var autoplayDelay = 2000;
var initialState;

function playForward(stepHistory) {
  var steps = stepHistory.length;
  var playId = setInterval(function() {
    if ((currentStep < steps) && playing) {
      $('#stepCounter').text(currentStep + 1);
      stepForward(stepHistory[currentStep]);
      currentStep++;
      if (currentStep >= steps) {
        $("#play-pause").find(">:first-child").attr("class", "fa fa-play");
        endReached = true;
        playing = false;
      }
    } else {
      $("#play-pause").find(">:first-child").attr("class", "fa fa-play");
      endReached = true;
      playing = false;
      clearInterval(playId);
    }
  }, autoplayDelay);
}

function playBackward(stepHistory) {
  if (currentStep == 1) {
    stepToInitialState();
    currentStep--;
  } else if (currentStep > 1) {
    stepForward(stepHistory[currentStep - 2]);
    currentStep--;
  }
  endReached = false;
  $('#stepCounter').text(currentStep);
}

function playOneStepForward(stepHistory) {
  var steps = stepHistory.length;
  if (currentStep < steps) {
    $('#stepCounter').text(currentStep + 1);
    stepForward(stepHistory[currentStep]);
    currentStep++;
  }
}

function stepForward(step) {
  if (currentStep != 0) {
    stepToInitialState();
  }

  step.textNode.forEach(function(d) {
    changeNodeText(d.node, d.text);
  });

  step.highlightNode.forEach(function(d) {
    setNodeColor(d.node, d.color, true);
  });

  step.textEdge.forEach(function(d) {
    changeEdgeText(d.start, d.end, d.text)
  });

  step.highlightEdge.forEach(function(d) {
    setEdgeColor(d.start, d.end, d.color, true)
  });
}

function stepToInitialState() {
  if (initialState == null) {
    return;
  }
  initialState.textNode.forEach(function(d) {
    changeNodeText(d.node, d.text);
  });

  initialState.highlightNode.forEach(function(d) {
    setNodeColor(d.node, d.color);
  });

  initialState.textEdge.forEach(function(d) {
    changeEdgeText(d.start, d.end, d.text)
  });

  initialState.highlightEdge.forEach(function(d) {
    setEdgeColor(d.start, d.end, d.color)
  });
}

function setInitialState() {
  initialState = {
    "textNode": [],
    "highlightNode": [],
    "textEdge": [],
    "highlightEdge": []
  };

  $("[id^=n]circle").each(function() {
    var nodeNumber = this.id.slice(1);
    initialState.textNode.push({
      "node": nodeNumber,
      "text": getNodeText(nodeNumber)
    });
    initialState.highlightNode.push({
      "node": nodeNumber,
      "color": getNodeColor(nodeNumber)
    });
  });

  $("[id^=uln],[id^=dln]").each(function() {
    var start = this.id.slice(3, this.id.indexOf("-"));
    var end = this.id.slice(this.id.indexOf("-") + 2);
    initialState.textEdge.push({
      "start": start,
      "end": end,
      "text": getEdgeText(start, end)
    });
    initialState.highlightEdge.push({
      "start": start,
      "end": end,
      "color": getEdgeColor(start, end)
    });
  })
}

function setNodeColor(node, color, thick) {
  $("#n" + node).css("stroke", color);
  if (thick) {
    $("#n" + node).css("stroke-width", 8);
  } else {
    $("#n" + node).css("stroke-width", 1);
  }
}

function setEdgeColor(start, end, color, thick) {
  var idString = "n" + start + "-n" + end;
  var edge;
  var arrow;
  if (!((edge = $("#ul" + idString)).length)) {
    if (!((edge = $("#uln" + end + "-n" + start)).length)) {
      edge = $("#dl" + idString);
      $("#a" + idString).css("fill", color);
    }
  }
  edge.css("stroke", color);
  if (thick) {
    edge.css("stroke-width", 5);
  } else {
    edge.css("stroke-width", 1);
  }
}

function changeEdgeText(start, end, text) {
  var edge;
  if (!((edge = $("#tn" + start + "-n" + end)).length)) {
    $("#tn" + end + "-n" + start).text(text);
  }
  edge.text(text);
}

function changeNodeText(node, text) {
  try {
    text.length;
    $("#nodeText" + node).text(text);
  } catch ( e ) {
    $("#nodeText" + node).text(" ");
  }
}

function getNodeText(node) {
  return $("#nodeText" + node).text();
}

function getNodeColor(node) {
  return $("#n" + node).css("stroke");
}

function getEdgeText(start, end) {
  var edge;
  if (!((edge = $("#tn" + start + "-n" + end)).length)) {
    return $("#tn" + end + "-n" + start).text();
  }
  return edge.text();
}

function getEdgeColor(start, end) {
  var idString = "n" + start + "-n" + end;
  var edge;
  if (!((edge = $("#ul" + idString)).length)) {
    if (!((edge = $("#uln" + end + "-n" + start)).length)) {
      return $("#dl" + idString).css("stroke")
    }
  }
  return edge.css("stroke");
}
