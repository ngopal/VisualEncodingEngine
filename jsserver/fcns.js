/**
 * Created by nikhilgopal on 6/25/16.
 */


//'node color (seq)',
//'node color (div)',
//'node color (cat)',
//'node border (quant)',
//'node border (bin)',
//'node border (cat)', //patterns
//'edge width (quant)',
//'edge width (bin)',
//'edge color (seq)',
//'edge color (div)',
//'edge color (cat)',
//'edge pattern (cat)',
//'edge arrow (cat)'

//var color = d3.scale.ordinal()
//    .domain([temp])
//    .range(['red','green']);

//only if numeric

function nodecolor_seq(domain,range) {
    return d3.scale.linear()
        .domain(domain)
        .range(range);
}

function nodecolor_div(domain,range) {
    return d3.scale.linear()
        .domain(domain)
        .range(range);
}

function nodecolor_cat(domain,range) {
    return d3.scale.ordinal()
        .domain(domain)
        .range(range);
}

function nodeshape_cat(domain) {
    var shapes = ["rectangle", "roundrectangle", "ellipse", "triangle", "pentagon", "hexagon", "heptagon", "octagon", "star", "diamond", "vee", "rhomboid"];
    return d3.scale.ordinal()
        .domain(domain)
        .range(shapes.slice(0,domain.length));
}

function edgepattern_cat(domain) {
    return d3.scale.ordinal()
        .domain(domain)
        .range(["solid", "dotted", "dashed"]);
}

//var nodecolor_seq = d3.scale.linear()
//    .domain([0,15,40])
//    .range(['red','green']);

//var nodecolor_div = d3.scale.linear()
//    .domain([0,15,40])
//    .range(['red','green']);

//var nodecolor_cat = d3.scale.ordinal()
//    .domain(['a', 'b'])
//    .range(['red','green']);

function nodeborder_quant(domain,range) {
    return d3.scale.ordinal()
        .domain(domain)
        .range(range);
};
function nodeborder_bin(domain,range) {
    return d3.scale.ordinal()
        .domain(domain)
        .range(range);
};
function nodeborder_cat(domain,range) {
    return d3.scale.ordinal()
        .domain(domain)
        .range(range);
};

function edgewidth_quant(domain,range) {
    return d3.scale.ordinal()
        .domain(domain)
        .range(range);
};
function edgewidth_bin(domain,range) {
    return d3.scale.ordinal()
        .domain(domain)
        .range(range);
};

function edgecolor_seq(domain,range) {
    return d3.scale.ordinal()
        .domain(domain)
        .range(range);
};

function edgecolor_div(domain,range) {
    return d3.scale.ordinal()
        .domain(domain)
        .range(range);
};

function edgecolor_cat(domain,range) {
    return d3.scale.ordinal()
        .domain(domain)
        .range(range);
};

function edgearrow_cat(domain,range) {
    return d3.scale.ordinal()
        .domain(domain)
        .range(range);
};

//var linestyle = d3.scale.ordinal()
//    .domain(["group1", "group2", "group3"])
//    .range(["solid", "dotted", "dashed"]);

//var nodeshape = d3.scale.ordinal()
//    .domain([0,1,2,3,4,5,6,7,8,9,10])
//    .range(["rectangle", "roundrectangle", "ellipse", "triangle", "pentagon", "hexagon", "heptagon", "octagon", "star", "diamond", "vee", "rhomboid"]);


function unitTest() {
    nodecolor_cat([1,2,3],['red','blue','green'])(1);
}