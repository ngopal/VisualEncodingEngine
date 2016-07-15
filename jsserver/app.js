var express = require('express');
var app = express();
var exec = require('child_process').exec;
var bodyParser = require('body-parser');
var jsonfile = require('jsonfile');


app.use(bodyParser.json()); // support json encoded bodies
app.use(bodyParser.urlencoded({ extended: true })); // support encoded bodies
app.use('/cytoscape.js', express.static(__dirname + '/cytoscape.js/dist/cytoscape.min.js'));
app.use('/d3.js', express.static(__dirname + '/node_modules/d3/d3.min.js'));
app.use('/jquery.js', express.static(__dirname + '/node_modules/jquery/dist/jquery.min.js'));
app.use('/colorbrewer.js', express.static(__dirname + '/node_modules/colorbrewer/colorbrewer.js'));
app.use('/fcns.js', express.static(__dirname + '/fcns.js'));
app.use('/networkgenerator.js', express.static(__dirname + '/networkgenerator.js'));
app.use('/cytoscape-cola.js', express.static(__dirname + '/node_modules/cytoscape-cola/cytoscape-cola.js'));
app.use('/datafiles.js', express.static(__dirname + '/datafiles.js'));
app.use('/ranktable.csv', express.static(__dirname + '/ranktable.csv'));
//app.use('/cola.js', express.static(__dirname + '/node_modules/cola/cola.js'));
//app.use('/require.js', express.static(__dirname + '/node_modules/requirejs/require.js'));
//app.use('/main.js', express.static(__dirname + '/main.js'));

app.get('/', function (req, res) {
    res.sendfile("/www/index.html", {root: __dirname});
});

app.get('/sfunction', function (req, res) {
    exec('Rscript r/silly.r this is a test', function(error, stdout, stderr) {
        if (error) {
            console.log(error);
            res.send(error);
        }
        else if (stderr) {
            console.log(stderr);
            res.send(stderr);
        }
        else if (stdout) {
            console.log("RAN SUCCESSFULLY");
            res.sendfile("savedoutput/test.json");
        }
    });
});

app.post('/lp', function (req, res) {
    var dataObject = req.body;
    var rdata = req.body.table;
    var assn = req.body.toAssign;
    var jfile = 'savedinput/' + Date.now() + '.json';
    var jfileout = 'savedoutput/' + Date.now() + '.json';
    jsonfile.writeFile(jfile, rdata, function (err, success) {
        if (err) {
            console.log(err);
        }
        else {


            exec('Rscript r/silly.r ' + jfile + ' ' + jfileout + ' ' + assn, function (error, stdout, stderr) {
                if (error) {
                    console.log(error);
                    res.send(error);
                }
                else if (stderr) {
                    console.log(stderr);
                    res.send(stderr);
                }
                else if (stdout) {
                    console.log("RAN SUCCESSFULLY");

                    jsonfile.readFile(jfileout, function (jrerr, jsoncontents) {
                        if (jrerr) {
                            console.log(jrerr);
                        }
                        else {
                            res.setHeader('Content-Type', 'application/json');
                            res.jsonp(jsoncontents);
                        }
                    });

                }
            });


        }
    });
});


//app.post('/convert', function (req, res) {
//    var rdata = req.body;
//    var jfile = 'savedinput/'+Date.now()+'.json';
//    var jfileout = 'savedoutput/'+Date.now()+'.json';
//    jsonfile.writeFile(jfile, rdata, function (err, success) {
//        if(err) {
//            console.log(err);
//        }
//        else {
//
//
//            exec('Rscript r/readAndConvert.r '+jfile+' '+jfileout, function(error, stdout, stderr) {
//                if (error) {
//                    console.log(error);
//                    res.send(error);
//                }
//                else if (stderr) {
//                    console.log(stderr);
//                    res.send(stderr);
//                }
//                else if (stdout) {
//                    console.log("RAN SUCCESSFULLY");
//
//                    jsonfile.readFile(jfileout, function(jrerr, jsoncontents) {
//                        if(jrerr) { console.log(jrerr); }
//                        else {
//                            res.setHeader('Content-Type', 'application/json');
//                            res.jsonp(jsoncontents);
//                        }
//                    });
//
//                }
//            });
//
//
//        }
//    });
//});

app.listen(3000, function () {
    console.log('Example app listening on port 3000!');
});