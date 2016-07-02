function getRandomIntInclusive(min, max) {
    return Math.floor(Math.random() * (max - min + 1)) + min;
}

function generateNetwork(numberOfNodes,numberOfEdges,nodeClasses,edgeClasses) {
    var items = [];
    for (var i = 0; i < numberOfNodes; i++) {
        var nodeClassChoice = function() {
            return nodeClasses[  getRandomIntInclusive(0,nodeClasses.length-1)  ];
        };
        var choice = nodeClassChoice();
        if (choice === "pathway") {
            items.push(  {data: {id: String(i), value: "group"+getRandomIntInclusive(1,3), usage: 'categorical', dimension: choice }}  )
        }
        else {
            items.push(  {data: {id: String(i), value: getRandomIntInclusive(1,100), usage: 'numeric', dimension: choice }}  )
        }
    }

    for (var k = 0; k < numberOfEdges; k++) {
        var edgeClassChoice = function() {
            return edgeClasses[  getRandomIntInclusive(0,edgeClasses.length-1)  ];
        };
        var choice = edgeClassChoice();
        if (choice === "coexpression") {
            items.push( {
                data: {
                    id: String(i+k),
                    source: getRandomIntInclusive(0,i),
                    target: getRandomIntInclusive(0,i),
                    dimension: 'coexpression',
                    value: getRandomIntInclusive(0,100),
                    usage: 'numeric'
                }
            }  )
        }
        else {
            items.push( {
                data: {
                    id: String(i+k),
                    source: getRandomIntInclusive(0,i),
                    target: getRandomIntInclusive(0,i),
                    dimension: 'probability',
                    value: Math.random()*100,
                    usage: 'numeric'
                }
            }  )
        }
    }

    console.log(items);
    return items;
}