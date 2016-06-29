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
        .domain( d3.extent(domain) )
        .range(range);
}

function nodecolor_div(domain,range) {
    return d3.scale.linear()
        .domain( d3.extent(domain) )
        .range(range);
}

function nodecolor_cat(domain,range) {
    return d3.scale.ordinal()
        .domain(domain)
        .range(range);
}

function nodeshape_cat(domain,range) {
    return d3.scale.ordinal()
        .domain(domain)
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
    //only takes numeric input, otherwise will throw undefined.
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
    return d3.scale.linear()
        .domain(domain)
        .range(range);
};
function edgewidth_bin(domain,range) {
    return d3.scale.linear()
        .domain(domain)
        .range(range);
};

function edgecolor_seq(domain,range) {
    return d3.scale.linear()
        .domain(  d3.extent(domain)  )
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


function unitTest(inNumericDomain, inCategoricalDomain, inNumericRange, inCategoricalRange) {
    var numericDomain = inNumericDomain || [1,2,3];
    var categoricalDomain = inCategoricalDomain || ["input1","input2","input3"];
    var numericRange = inNumericRange || [100,200,300];
    var categoricalRange = inCategoricalRange || ["output1", "output2", "output3"];

    // Node Color
    console.log(  ["nodecolor_seq", "numericInput", "numericOutput", nodecolor_seq(numericDomain,numericRange)(1), "UNUSED", "PASS"]  );
    console.log(  ["nodecolor_seq", "numericInput", "categoricalOutput", nodecolor_seq(numericDomain,categoricalRange)(1), "USED", "PASS"]  );
    console.log(  ["nodecolor_seq", "categoricalInput", "numericOutput", nodecolor_seq(categoricalDomain,numericRange)("input1"), "UNUSED", "FAIL"]  );
    console.log(  ["nodecolor_seq", "categoricalInput", "categoricalOutput", nodecolor_seq(categoricalDomain,categoricalRange)("input1"), "UNUSED", "FAIL"]  );
    //nodecolor_seq, input: [numeric], output: [categorical]

    console.log(  ["nodecolor_div", "numericInput", "numericOutput", nodecolor_div(numericDomain,numericRange)(1), "UNUSED", "PASS"]  );
    console.log(  ["nodecolor_div", "numericInput", "categoricalOutput", nodecolor_div(numericDomain,categoricalRange)(1), "USED", "PASS"]  );
    console.log(  ["nodecolor_div", "categoricalInput", "numericOutput", nodecolor_div(categoricalDomain,numericRange)("input1"), "UNUSED", "FAIL"]  );
    console.log(  ["nodecolor_div", "categoricalInput", "categoricalOutput", nodecolor_div(categoricalDomain,categoricalRange)("input1"), "UNUSED", "FAIL"]  );
    //nodecolor_div, input: [numeric], output: [categorical]

    console.log(  ["nodecolor_cat", "numericInput", "numericOutput", nodecolor_cat(numericDomain,numericRange)(1), "UNUSED", "PASS"]  );
    console.log(  ["nodecolor_cat", "numericInput", "categoricalOutput", nodecolor_cat(numericDomain,categoricalRange)(1), "USED", "PASS"]  );
    console.log(  ["nodecolor_cat", "categoricalInput", "numericOutput", nodecolor_cat(categoricalDomain,numericRange)("input1"), "UNUSED", "PASS"]  );
    console.log(  ["nodecolor_cat", "categoricalInput", "categoricalOutput", nodecolor_cat(categoricalDomain,categoricalRange)("input1"), "USED", "PASS"]  );
    //nodecolor_cat, input: [numeric, categorical], output: [categorical]


    // Node Border
    console.log(  ["nodeborder_quant", "numericInput", "numericOutput", nodeborder_quant(numericDomain,numericRange)(1), "USED", "PASS"]  );
    console.log(  ["nodeborder_quant", "numericInput", "categoricalOutput", nodeborder_quant(numericDomain,categoricalRange)(1), "UNUSED", "PASS"]  );
    console.log(  ["nodeborder_quant", "categoricalInput", "numericOutput", nodeborder_quant(categoricalDomain,numericRange)("input1"), "UNUSED", "FAIL"]  );
    console.log(  ["nodeborder_quant", "categoricalInput", "categoricalOutput", nodeborder_quant(categoricalDomain,categoricalRange)("input1"), "UNUSED", "FAIL"]  );
    //nodeborder_quant, input: [numeric], output: [numeric]

    console.log(  ["nodeborder_bin", "numericInput", "numericOutput", nodeborder_bin(numericDomain,numericRange)(1), "UNUSED", "PASS"]  );
    console.log(  ["nodeborder_bin", "numericInput", "categoricalOutput", nodeborder_bin(numericDomain,categoricalRange)(1), "USED", "PASS"]  );
    console.log(  ["nodeborder_bin", "categoricalInput", "numericOutput", nodeborder_bin(categoricalDomain,numericRange)("input1"), "UNUSED", "FAIL"]  );
    console.log(  ["nodeborder_bin", "categoricalInput", "categoricalOutput", nodeborder_bin(categoricalDomain,categoricalRange)("input1"), "UNUSED", "FAIL"]  );
    //nodeborder_bin, input: [numeric], output: [categorical]

    console.log(  ["nodeborder_cat", "numericInput", "numericOutput", nodeborder_cat(numericDomain,numericRange)(1), "UNUSED", "PASS"]  );
    console.log(  ["nodeborder_cat", "numericInput", "categoricalOutput", nodeborder_cat(numericDomain,categoricalRange)(1), "USED", "PASS"]  );
    console.log(  ["nodeborder_cat", "categoricalInput", "numericOutput", nodeborder_cat(categoricalDomain,numericRange)("input1"), "UNUSED", "PASS"]  );
    console.log(  ["nodeborder_cat", "categoricalInput", "categoricalOutput", nodeborder_cat(categoricalDomain,categoricalRange)("input1"), "USED", "PASS"]  );
    //nodeborder_cat, input: [numeric, categorical], output: [categorical]

    // Node Shape
    console.log(  ["nodeshape_cat", "numericInput", "numericOutput", nodeshape_cat(numericDomain,numericRange)(1), "UNUSED", "PASS"]  );
    console.log(  ["nodeshape_cat", "numericInput", "categoricalOutput", nodeshape_cat(numericDomain,categoricalRange)(1), "USED", "PASS"]  );
    console.log(  ["nodeshape_cat", "categoricalInput", "numericOutput", nodeshape_cat(categoricalDomain,numericRange)("input1"), "UNUSED", "PASS"]  );
    console.log(  ["nodeshape_cat", "categoricalInput", "categoricalOutput", nodeshape_cat(categoricalDomain,categoricalRange)("input1"), "USED", "PASS"]  );
    //nodeshape_cat, input: [numeric, categorical], output: [categorical]

    // Edge Width
    console.log(  ["edgewidth_quant", "numericInput", "numericOutput", edgewidth_quant(numericDomain,numericRange)(1), "USED", "PASS"]  );
    console.log(  ["edgewidth_quant", "numericInput", "categoricalOutput", edgewidth_quant(numericDomain,categoricalRange)(1), "UNUSED", "PASS"]  );
    console.log(  ["edgewidth_quant", "categoricalInput", "numericOutput", edgewidth_quant(categoricalDomain,numericRange)("input1"), "UNUSED", "FAIL"]  );
    console.log(  ["edgewidth_quant", "categoricalInput", "categoricalOutput", edgewidth_quant(categoricalDomain,categoricalRange)("input1"), "UNUSED", "FAIL"]  );
    //edgewidth_quant, input: [numeric], output: [numeric]

    console.log(  ["edgewidth_bin", "numericInput", "numericOutput", edgewidth_bin(numericDomain,numericRange)(1), "UNUSED", "PASS"]  );
    console.log(  ["edgewidth_bin", "numericInput", "categoricalOutput", edgewidth_bin(numericDomain,categoricalRange)(1), "USED", "PASS"]  );
    console.log(  ["edgewidth_bin", "categoricalInput", "numericOutput", edgewidth_bin(categoricalDomain,numericRange)("input1"), "UNUSED", "FAIL"]  );
    console.log(  ["edgewidth_bin", "categoricalInput", "categoricalOutput", edgewidth_bin(categoricalDomain,categoricalRange)("input1"), "UNUSED", "FAIL"]  );
    //edgewidth_bin, input: [numeric], output: [categorical]

    // Edge Color
    console.log(  ["edgecolor_seq", "numericInput", "numericOutput", edgecolor_seq(numericDomain,numericRange)(1), "UNUSED", "PASS"]  );
    console.log(  ["edgecolor_seq", "numericInput", "categoricalOutput", edgecolor_seq(numericDomain,categoricalRange)(1), "USED", "PASS"]  );
    console.log(  ["edgecolor_seq", "categoricalInput", "numericOutput", edgecolor_seq(categoricalDomain,numericRange)("input1"), "UNUSED", "FAIL"]  );
    console.log(  ["edgecolor_seq", "categoricalInput", "categoricalOutput", edgecolor_seq(categoricalDomain,categoricalRange)("input1"), "UNUSED", "FAIL"]  );
    //edgecolor_seq, input: [numeric], output: [categorical]

    console.log(  ["edgecolor_div", "numericInput", "numericOutput", edgecolor_div(numericDomain,numericRange)(1), "UNUSED", "PASS"]  );
    console.log(  ["edgecolor_div", "numericInput", "categoricalOutput", edgecolor_div(numericDomain,categoricalRange)(1), "USED", "PASS"]  );
    console.log(  ["edgecolor_div", "categoricalInput", "numericOutput", edgecolor_div(categoricalDomain,numericRange)("input1"), "UNUSED", "FAIL"]  );
    console.log(  ["edgecolor_div", "categoricalInput", "categoricalOutput", edgecolor_div(categoricalDomain,categoricalRange)("input1"), "UNUSED", "FAIL"]  );
    //edgecolor_div, input: [numeric], output: [categorical]

    console.log(  ["edgecolor_cat", "numericInput", "numericOutput", edgecolor_cat(numericDomain,numericRange)(1), "UNUSED", "PASS"]  );
    console.log(  ["edgecolor_cat", "numericInput", "categoricalOutput", edgecolor_cat(numericDomain,categoricalRange)(1), "USED", "PASS"]  );
    console.log(  ["edgecolor_cat", "categoricalInput", "numericOutput", edgecolor_cat(categoricalDomain,numericRange)("input1"), "UNUSED", "PASS"]  );
    console.log(  ["edgecolor_cat", "categoricalInput", "categoricalOutput", edgecolor_cat(categoricalDomain,categoricalRange)("input1"), "USED", "PASS"]  );
    //edgecolor_cat, input: [numeric, categorical], output: [categorical]

    // Edge Pattern
    console.log(  ["edgepattern_cat", "numericInput", "numericOutput", edgepattern_cat(numericDomain,numericRange)(1), "UNUSED", "PASS"]  );
    console.log(  ["edgepattern_cat", "numericInput", "categoricalOutput", edgepattern_cat(numericDomain,categoricalRange)(1), "USED", "PASS"]  );
    console.log(  ["edgepattern_cat", "categoricalInput", "numericOutput", edgepattern_cat(categoricalDomain,numericRange)("input1"), "UNUSED", "PASS"]  );
    console.log(  ["edgepattern_cat", "categoricalInput", "categoricalOutput", edgepattern_cat(categoricalDomain,categoricalRange)("input1"), "USED", "PASS"]  );
    //edgepattern_cat, input: [numeric, categorical], output: [categorical]

    // Edge Arrow
    console.log(  ["edgearrow_cat", "numericInput", "numericOutput", edgearrow_cat(numericDomain,numericRange)(1), "UNUSED", "PASS"]  );
    console.log(  ["edgearrow_cat", "numericInput", "categoricalOutput", edgearrow_cat(numericDomain,categoricalRange)(1), "USED", "PASS"]  );
    console.log(  ["edgearrow_cat", "categoricalInput", "numericOutput", edgearrow_cat(categoricalDomain,numericRange)("input1"), "UNUSED", "PASS"]  );
    console.log(  ["edgearrow_cat", "categoricalInput", "categoricalOutput", edgearrow_cat(categoricalDomain,categoricalRange)("input1"), "USED", "PASS"]  );
    //edgearriw_cat, input: [numeric, categorical], output: [categorical]

}