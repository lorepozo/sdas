/**
 * Created by Moan on 10/05/16.
 */

function save() {
    var algorithm = addAlgorithmToLocalStorage("");
    addDropdownOption(algorithm[0], algorithm[1]);
}

function addDropdownOption(algName, algorithm) {
    $("#algorithm-dropdown").append("<li><a id=\"" + algName + "\">" + algName + "</a></li>");
    $("#" + algName).click(function(){
        editor.getDoc().setValue(algorithm);
        if (!maximized) { editor.getWrapperElement().style.display = "none"; }
        $("#dropdown").html(algName + " <span class=\"caret\"></span>");
    });
}

function addAlgorithmToLocalStorage(algName) {
    var algorithms;
    var storage = window.localStorage;
    (storage.algorithms == undefined) ? algorithms = [] : algorithms = JSON.parse(storage.getItem("algorithms"));
    (algName.length == 0) && (algName = "my_algorithm" + algorithms.length);

    var algorithm = [algName, editor.getDoc().getValue()];
    algorithms.push(algorithm);
    storage.setItem("algorithms", JSON.stringify(algorithms));
    $.jAlert({
        'title': 'Success',
        'content': 'Algorithm saved. You can find it in the algorithm dropdown as ' + algName,
        'theme': 'green',
        'btns': { 'text': 'Got it!' },
        'closeOnEsc': true,
        'closeBtn': false
    });
    return algorithm;
}

function saveAs() {
    $.jAlert({
        'title': 'Save as',
        'content': '<form>File name:<br><input type="text" name="file_name"></form>',
        'theme' : 'dark_blue',
        'autofocus': 'input[name="file_name"]',
        'btns': {'text' : 'Save', 'theme' : 'green', 'closeAlert': false, 'onClick': function(e) {
            e.preventDefault();
            var alert = $('#' + this.id).parents('.jAlert');
            var fileName = alert.find('form').find('input[name="file_name"]').val();


            if (fileName != null) {
                var algorithm = addAlgorithmToLocalStorage(fileName);
                addDropdownOption(fileName, algorithm[1]);
            }

            alert.closeAlert();
            return false;
        }}
    });
}

function clearLocalStorage() {
    window.localStorage.clear();
}

function addAlgorithmsFromStorage() {
    var storage = window.localStorage;
    if (storage.algorithms == undefined){ return; }
    var algorithms = JSON.parse(storage.algorithms);
    algorithms.forEach(function(alg) {
        $("#algorithm-dropdown").append("<li><a>" + alg[0] + "</a></li>");
        $("#algorithm-dropdown:last-child").click(function() {
            editor.getDoc().setValue(alg[1]);
            if (!maximized) { editor.getWrapperElement().style.display = "none"; }
            $("#dropdown").html(alg[0] + " <span class=\"caret\"></span>");
        });
    })
}