/* algorithm, scheme interface */

function Algorithm(o) {
  this.code = o.code;
  this.graph = o.graph;
  this.rounds = parseInt(o.rounds) || 15;
  this.start = parseInt(o.start) || 0;
}

Algorithm.prototype.compute = function() {
  var _this = this;
  if (_this.code === undefined) {
    return;
  }
  var x = new XMLHttpRequest();
  x.open("GET", "scheme/jsinterface.scm", true);
  x.onload = function() {
    interpreter.evaluate(
      this.responseText +
      " " + _this.code + " " +
      "(output-parser (runtime (input-parser)))");
  };
  x.send();
};

/* depends on globals: step, stepHistory, stepNumber, algorithm, adjacencyMatrix */

BiwaScheme.define_libfunc("js-to-scheme", 0, 0, function() {
  var result = [adjacencyMatrix, algorithm.rounds, algorithm.start];
  return BiwaScheme.deep_array_to_list(result);
});

BiwaScheme.define_libfunc("scheme-to-js", 3, 3, function(args) {
  if (args[2] > stepNumber) {
    step = {
      textNode: [],
      textEdge: [],
      highlightNode: [],
      highlightEdge: []
    };
    stepHistory.push(step);
    stepNumber++;
  }

  switch (args[1]) {
    case 0:
      step.highlightNode.push({
        node: args[0][0],
        color: args[0][1]
      });
      break;
    case 1:
      step.textNode.push({
        node: args[0][0],
        text: args[0][1]
      });
      break;
    case 2:
      step.highlightEdge.push({
        start: args[0][0],
        end: args[0][1],
        color: args[0][2]
      });
      break;
    case 3:
      step.textEdge.push({
        start: args[0][0],
        end: args[0][1],
        text: args[0][2]
      });
  }
});

