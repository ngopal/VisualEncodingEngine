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

function nodeshape_cat(domain,range) {
    //var shapes = ["rectangle", "roundrectangle", "ellipse", "triangle", "pentagon", "hexagon", "heptagon", "octagon", "star", "diamond", "vee", "rhomboid"];
    return d3.scale.ordinal()
        .domain(domain)
        //.range(shapes.slice(0,domain.length));
        .range(range);
}

function edgepattern_cat(domain,range) {
    return d3.scale.ordinal()
        .domain(domain)
        .range(range);
}

function nodeborder_quant(domain,range) {
    return d3.scale.linear()
        .domain(domain)
        .range(range);
};
function nodeborder_bin(domain,range) {
    return d3.scale.quantize()
        .domain(domain)
        .range(range);
};
function nodeborder_cat(domain,range) {
    return d3.scale.ordinal()
        .domain(domain)
        .range(range);
};

function edgewidth_quant(domain,range) {
    return d3.scale.linear()
        .domain(domain)
        .range(range);
};
function edgewidth_bin(domain,range) {
    return d3.scale.quantize()
        .domain(domain)
        .range(range);
};

function edgecolor_seq(domain,range) {
    return d3.scale.linear()
        .domain(domain)
        .range(range);
};

function edgecolor_div(domain,range) {
    return d3.scale.linear()
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


function unitTest() {
    nodecolor_cat([1,2,3],['red','blue','green'])(1);
}