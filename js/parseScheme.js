/**
 * Created by Moan on 04/05/16.
 */


var stepNumber = 0;

BiwaScheme.define_libfunc("scheme-to-js", 3, 3, function(args) {
    if(args[2] > stepNumber) {
        step = {"textNode" : [], "textEdge" : [], "highlightNode" : [], "highlightEdge" : []};
        stepHistory.push(step);
        stepNumber++;
    }

    switch(args[1]) {
        case 0:
            step.highlightNode.push({"node": args[0][0], "color": args[0][1]});
            break;
        case 1:
            step.textNode.push({"node": args[0][0], "text": args[0][1]});
            break;
        case 2:
            step.highlightEdge.push({"start": args[0][0], "end": args[0][1], "color": args[0][2]});
            break;
        case 3:
            step.textEdge.push({"start": args[0][0], "end": args[0][1], "text": args[0][2]});
    }
});

BiwaScheme.define_libfunc("js-to-scheme", 0, 0, function() {
    return BiwaScheme.deep_array_to_list(adjacencyMatrix);
});

