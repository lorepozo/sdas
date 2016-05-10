/**
 * Created by Moan on 04/05/16.
 */

var algorithm;

var interpreter = new BiwaScheme.Interpreter(function(e) {
    console.error(e.message);
});

function compute() {
    var x = new XMLHttpRequest();
    x.open("GET", "scheme/schemeToJS.scm", true);
    x.onload = function() {
        interpreter.evaluate(this.responseText + " " + algorithm + " " + "(output-parser (runtime (input-parser)))");
    };
    x.send();
}

