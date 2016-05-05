/**
 * Created by Moan on 04/05/16.
 */

var interpreter = new BiwaScheme.Interpreter(function(e) {
    console.log(e.message);
});

function compute() {
    var x = new XMLHttpRequest();
    x.open("GET", "scheme/schemeToJS.scm", true);
    x.onload = function() {
        interpreter.evaluate(this.responseText);
    }
    x.send();
}


