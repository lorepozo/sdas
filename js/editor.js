/* code editor */

function addEditor() {
  var textArea = document.getElementById("editorArea");
  var editor = CodeMirror.fromTextArea(textArea, {
    mode: "scheme",
    lineNumbers: true
  });

  editor.parent = $(textArea).parent();
  var width = editor.parent.parent().width();
  editor.setSize(width - 10, 500);
  $('#textEditorBorder').hide();

  initDropdownOptions();

  return editor;
}

function initDropdownOptions() {
  $("#algorithm-dropdown").find('li > a').each(function() {
    $(this).click(function() {
      $("#dropdown").html($(this).html() + ' <span class="caret"></span>');
      var fileName = $(this).html().toLowerCase().replace(/\s/g, '_');
      $.get('scheme/algs/' + fileName + '.scm', function(data) {
        editor.getDoc().setValue(';;; Scheme code editor ;;;\n' + data);
      });
    })
  });
  var storedAlgorithms = JSON.parse(localStorage.getItem("algorithms")) || [];
  storedAlgorithms.forEach(addStoredDropdownOption);
}

/* local storage handling */

function save(fileName) {
  var storedAlgorithm = addAlgorithmToLocalStorage(fileName);
  addStoredDropdownOption(storedAlgorithm);
}

function saveAs() {
  $.jAlert({
    title: 'Save as',
    content: '<form>File name:<br><input type="text" name="file_name"></form>',
    theme: 'dark_blue',
    autofocus: 'input[name="file_name"]',
    btns: {
      text: 'Save',
      theme: 'green',
      closeAlert: false,
      onClick: function(e) {
        e.preventDefault();
        var alert = $('#' + this.id).parents('.jAlert');
        var fileName = alert.find('form').find('input[name="file_name"]').val();

        save(fileName);

        alert.closeAlert();
        return false;
      }
    }
  });
}

function findStoredAlgorithm(name) {
  var storedAlgorithms = JSON.parse(localStorage.getItem("algorithms")) || [];
  for (var i = 0; i < storedAlgorithms.length; i++) {
    if (storedAlgorithms[i].name == name) {
      return storedAlgorithms[i];
    }
  }
}

function addStoredDropdownOption(storedAlgorithm) {
  $("#algorithm-dropdown").append("<li><a>" + storedAlgorithm.name + "</a></li>");
  $("#algorithm-dropdown > li:last-child > a").click(function() {
    var storedAlgorithm = findStoredAlgorithm($(this).html());
    if (storedAlgorithm === undefined) {
      return;
    }
    editor.getDoc().setValue(storedAlgorithm.content);
    $("#dropdown").html(storedAlgorithm.name + " <span class=\"caret\"></span>");
  });
}

function addAlgorithmToLocalStorage(algName) {
  algName = algName || ("my_algorithm" + storedAlgorithms.length);
  var storedAlgorithm = {
    name: algName,
    content: editor.getDoc().getValue(),
  };

  var storedAlgorithms = JSON.parse(localStorage.getItem("algorithms")) || [];
  storedAlgorithms.push(storedAlgorithm);
  localStorage.setItem("algorithms", JSON.stringify(storedAlgorithms));
  $.jAlert({
    'title': 'Success',
    'content': 'Algorithm saved. You can find it in the algorithm dropdown as "' + algName + '"',
    'theme': 'green',
    'btns': {
      'text': 'Got it!'
    },
    'closeOnEsc': true,
    'closeBtn': false
  });
  return storedAlgorithm;
}

