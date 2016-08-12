var express = require('express');
var app = express();
var exec = require('child_process').exec;
var bodyParser = require('body-parser');
var jsonfile = require('jsonfile');
var d3 = require('d3');
var MongoClient = require('mongodb').MongoClient
    , assert = require('assert');


// MongoDB functions and parameter definitions
var mongourl = 'mongodb://localhost:27017/pilot';
function insertOneItem(database, user, page, time, dataObject) {
    var collection = database.collection('evaldata');
    var user = user;
    var page = page;
    collection.insertOne({
        user : user,
        page : page,
        time : time,
        eleid : dataObject.id,
        name : dataObject.name,
        nodeEncoding1 : dataObject.Nencoding1,
        nodeEncoding2 : dataObject.Nencoding2,
        edgeEncoding1 : dataObject.Eencoding1,
        edgeEncoding2 : dataObject.Eencoding2,
        nodebackground : dataObject.results[0],
        nodeshape : dataObject.results[1],
        nodeborderwidth : dataObject.results[2],
        nodeheight : dataObject.results[3],
        nodewidth : dataObject.results[4],
        linecolor : dataObject.results[5],
        linestyle : dataObject.results[6],
        xposition : dataObject.position[0],
        yposition : dataObject.position[1],
        selected : dataObject.selected,
        participantResponse : dataObject.participantResponse,
        browser : dataObject.browser,
        clickX : dataObject.clickX,
        clickY : dataObject.clickY,
        windowheight : dataObject.windowheight,
        windowwidth : dataObject.windowwidth,
        time : dataObject.time,
        network : dataObject.network
}, function(insertErr, insertResult) {
        assert.equal(insertErr, null);
        assert(1, insertResult.result.n);
        assert(1, insertResult.ops.length);
    });
}
// End MongoDB functions and parameter definitions

app.use(bodyParser.json()); // support json encoded bodies
app.use(bodyParser.urlencoded({ extended: true })); // support encoded bodies
app.use('/cytoscape.js', express.static(__dirname + '/cytoscape.min.js'));
app.use('/d3.js', express.static(__dirname + '/node_modules/d3/d3.min.js'));
app.use('/jquery.js', express.static(__dirname + '/node_modules/jquery/dist/jquery.min.js'));
app.use('/colorbrewer.js', express.static(__dirname + '/node_modules/colorbrewer/colorbrewer.js'));
app.use('/fcns.js', express.static(__dirname + '/fcns.js'));
app.use('/cache.js', express.static(__dirname + '/cache.js'));
app.use('/networkgenerator.js', express.static(__dirname + '/networkgenerator.js'));
app.use('/cytoscape-cola.js', express.static(__dirname + '/node_modules/cytoscape-cola/cytoscape-cola.js'));
app.use('/datafiles.js', express.static(__dirname + '/datafiles.js'));
app.use('/ranktable.csv', express.static(__dirname + '/ranktable.csv'));
app.use('/bootstrap.js', express.static(__dirname + '/node_modules/bootstrap/dist/js/bootstrap.min.js'));
app.use('/bootstrap.css', express.static(__dirname + '/node_modules/bootstrap/dist/css/bootstrap.min.css'));
app.use('/survey.css', express.static(__dirname + '/surveyjs/survey.css'));
app.use('/survey.js', express.static(__dirname + '/surveyjs/survey.js'));
app.use('/knockout.js', express.static(__dirname + '/node_modules/knockout/build/output/knockout-latest.js'));
app.use('/jqueryui.js', express.static(__dirname + '/node_modules/jquery-ui/ui/jquery-1-7.js'));
app.use('/dialog.css', express.static(__dirname + '/node_modules/jquery-ui/themes/base/dialog.css'));
app.use('/randomnetworks.json', express.static(__dirname + '/randomnetworks.json'));


app.get('/', function (req, res) {
    res.sendfile("/www/index.html", {root: __dirname});
});

app.get('/participant', function (req, res) {
    res.sendfile("/www/participantview.html", {root: __dirname});
});

app.get('/tutorial', function (req, res) {
    res.sendfile("/www/tutorial.html", {root: __dirname});
});

app.get('/onmobile', function (req, res) {
    res.sendfile("/www/redirectmobile.html", {root: __dirname});
});

app.get('/thanks', function (req, res) {
    res.sendfile("/www/thankyou.html", {root: __dirname});
});

app.get('/demographic', function (req, res) {
    res.sendfile("/www/demographic.html", {root: __dirname});
});

app.get('/consent', function (req, res) {
    res.sendfile("/www/consent.html", {root: __dirname});
});

app.get('/nodeprompt', function (req, res) {
    res.sendfile("/www/nodeprompt.html", {root: __dirname});
});

app.get('/edgeprompt', function (req, res) {
    res.sendfile("/www/edgeprompt.html", {root: __dirname});
});

app.get('/clear', function (req, res) {
    res.sendfile("/www/quickcacheclear.html", {root: __dirname});
});

app.get('/rand', function (req, res) {
    var weight_hash = {'random':0.5, 'baseline':0.5};
    var rand = (function() {
        if (Math.random() <= weight_hash.baseline) {
            return "Baseline";
        } else {
            return "Random";
        }
    })();
    res.send(JSON.stringify({'randomGroup': rand}));
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

app.post('/submitdata', function(req, res) {
    var dataObject = req.body.data;
    var dtime = Date.now();
    var jfile = 'savedData/' + req.body.user + '-' + dtime + '-' + req.body.page + '.json';

    MongoClient.connect(mongourl, function(dberr, db) {
        assert.equal(null, dberr);
        console.log("Connected succesfully to server");

        insertOneItem(db, req.body.user, req.body.page, dtime, dataObject);
        db.close()
    });

    jsonfile.writeFile(jfile, dataObject, function(err, success) {
        if(err) {
            console.log(err);
        }
        else {
            console.log(success);
            res.setHeader('Content-Type', 'application/json');
            res.jsonp({'response':'success'});
        }
    });
});

app.post('/submitsurveydata', function(req, res) {
    var dataObject = req.body.data;
    var dtime = Date.now();
    var jfile = 'savedData/' + req.body.user + '-' + dtime + '-' + req.body.page + '.json';

    MongoClient.connect(mongourl, function(dberr, db) {
        assert.equal(null, dberr);
        console.log("Connected succesfully to server");

        insertOneItem(db, req.body.user, 'survey', dtime, dataObject);
        db.close()
    });


    jsonfile.writeFile(jfile, dataObject, function(err, success) {
        if(err) {
            console.log(err);
        }
        else {
            console.log(success);
            res.setHeader('Content-Type', 'application/json');
            res.jsonp({'response':'success'});
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

// Serving each combination page individually
d3.range(1,29).map(function(i) {
    return app.get('/page' + i, function (req, res) {
        res.sendfile("/www/nodes/page"+i+".html", {root: __dirname});
    });
});

d3.range(1,16).map(function(i) {
    return app.get('/page' + (i+28), function (req, res) {
        res.sendfile("/www/edges/page"+i+".html", {root: __dirname});
    });
});

//app.get('/nodes/page/:confignumber', function (req, res) {
//    var configure = req.params.confignumber;
//    console.log("FOUND THIS PARAMETER "+configure);
//    //res.send(JSON.stringify({'page': configure}));
//    res.send("Oh, hello, "+configure);
//});

app.listen(3000, function () {
    console.log('Example app listening on port 3000!');
});