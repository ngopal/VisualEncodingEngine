/**
 * Created by nikhilgopal on 9/2/16.
 */

function testAll(r) {
//        var r = r || 0;
    var report = {};
    var numNodeAttrs = thing.map(function(n){ if(n.type==="node"){ return n.style; } }).filter(function(n){ return n != undefined }).length;
    var numEdgeAttrs = thing.map(function(n){ if(n.type==="edge"){ return n.style; } }).filter(function(n){ return n != undefined }).length;
    report.network = sessionStorage.getItem('netToServe');

    // Nodes
    report.nodeInputVals = cy.nodes().map(function(k){
        return +k.data().value || k.data().value;
    });
    report.nodeInputRange = d3.extent(report.nodeInputVals);
    report.nodeOutputVals =  d3.range(0,numNodeAttrs).map(function(q){
        return cy.nodes().map(function(k){
            //iterate over various things
            return thing[q].func(d3.extent(cy.nodes().map(function(n){
                //iterate over various color ranges
                return +n.data().value || n.data().value;
            })), (Array.isArray(thing[r].range)) ? thing[r].range : thing[r].range[3]  )(+k.data().value || k.data().value);
        });
    });
    report.nodeOutputRange = report.nodeOutputVals.map(function(o){ return d3.extent(o);});
    report.nodeFuncs = d3.range(0,numNodeAttrs).map(function(q){return thing[q].func;});

    // Edges
    report.edgeInputVals = cy.edges().map(function(k){
        return +k.data().value || k.data().value;
    });
    report.edgeInputRange = d3.extent(report.edgeInputVals);
    report.edgeOutputVals =  d3.range(0,numEdgeAttrs).map(function(q){
        return cy.edges().map(function(k){
            //iterate over various things
            return thing[q].func(d3.extent(cy.nodes().map(function(n){
                //iterate over various color ranges
                return +n.data().value || n.data().value;
            })), (Array.isArray(thing[r].range)) ? thing[r].range : thing[r].range[3]  )(+k.data().value || k.data().value);
        });
    });
    report.edgeOutputRange = report.edgeOutputVals.map(function(o){ return d3.extent(o);});
    report.edgeFuncs = d3.range(0,numEdgeAttrs).map(function(q){return thing[q].func;});

    return report;
}

function printColorRangeOut(arrNum) {
    for (var i = 0; i < arrNum.length;i++) {
        console.log('%c'+arrNum[i],     'color:'+arrNum[i]+';');
    }
}

function printSizeRangeOut(arrNum) {
    for (var i = 0; i < arrNum.length;i++) {
        console.log('%c'+arrNum[i],     'font-size:'+arrNum[i]*2+'px;');
    }
}

function testEachNodes(r) {
    var report = {};
    report.network = sessionStorage.getItem('netToServe');

    // Element
    report.nodeInputVals = cy.nodes().map(function(k){
        return +k.data().value || k.data().value;
    });
    report.nodeInputRange = d3.extent(report.nodeInputVals);
    report.nodeOutputVals = report.nodeInputVals.map(function(q) {
        return thing[r].func(d3.extent(report.nodeInputVals),
            (Array.isArray(thing[r].range)) ? thing[r].range : thing[r].range[3]  )(q)
    });
    report.nodeOutputRange = d3.extent(report.nodeOutputVals);
    report.nodeFunc = thing[r].func;

    return report;
}

function testEachEdges(r) {
    var report = {};
    report.network = sessionStorage.getItem('netToServe');

    // Element
    report.edgeInputVals = cy.edges().map(function(k){
        return +k.data().value || k.data().value;
    });
    report.edgeInputRange = d3.extent(report.edgeInputVals);
    report.edgeOutputVals = report.edgeInputVals.map(function(q) {
        return thing[r].func(d3.extent(report.edgeInputVals),
            (Array.isArray(thing[r].range)) ? thing[r].range : thing[r].range[3]  )(q)
    });
    report.edgeOutputRange = d3.extent(report.edgeOutputVals);
    report.edgeFunc = thing[r].func;

    return report;
}