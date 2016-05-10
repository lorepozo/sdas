/**
 * Created by Moan on 05/05/16.
 */

var editor;

function addEditor() {
    var textArea = document.getElementById("editorArea");
    editor = CodeMirror.fromTextArea(textArea, {
        mode:  "scheme",
        lineNumbers: true
    });

    var width = $(textArea).parent().parent().width();
    editor.setSize(width-10, 500);
    editor.getWrapperElement().style.display = "none";
}



function addDefaultDropdownOptions() {
    var dropdown = document.getElementById("algorithm-dropdown");
    [].forEach.call(dropdown.children, function(choice) {
        choice.firstElementChild.onclick = function() {
            var fileName = this.innerHTML.toLowerCase().replace(/\s/g, "_");
            var x = new XMLHttpRequest();
            x.open("GET", "scheme/algs/" + fileName + ".scm", true);
            x.onload = function() {
                editor.getWrapperElement().style.display = "inherit";
                editor.getDoc().setValue(" ;;; Scheme code editor ;;;\n " + this.responseText);
                if (!maximized) { editor.getWrapperElement().style.display = "none"; }
            };
            x.send();
            $("#dropdown").html(this.innerHTML + " <span class=\"caret\"></span>");
        }
    })
}

// initialize with leader elect
var x = new XMLHttpRequest();
x.open("GET", "scheme/algs/luby_mis.scm", true);
x.onload = function() {
    editor.getDoc().setValue(";;; Scheme code editor ;;;\n " + this.responseText);
};
x.send();

