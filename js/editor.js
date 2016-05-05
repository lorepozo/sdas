/**
 * Created by Moan on 05/05/16.
 */

var editor;
addEditor();

function addEditor() {
    var textArea = document.getElementById("editorArea");
    editor = CodeMirror.fromTextArea(textArea, {
        mode:  "scheme",
        lineNumbers: true
    });

    var width = $(textArea).parent().parent().width();
    editor.setSize(width-10, 500);
}

function addDropdownHandlers() {

}

addDropdownOptions();

function addDropdownOptions() {
    var dropdown = document.getElementById("algorithm-dropdown");
    [].forEach.call(dropdown.children, function(choice) {
        choice.firstElementChild.onclick = function() {
            var fileName = this.innerHTML.toLowerCase().replace(/\s/g, "_");
            var x = new XMLHttpRequest();
            x.open("GET", "scheme/algs/" + fileName + ".scm", true);
            x.onload = function() {
                editor.getDoc().setValue(";;; Scheme code editor ;;;\n " + this.responseText);
            }
            x.send();
        }
    })
}

