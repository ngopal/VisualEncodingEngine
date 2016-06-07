var express = require('express');
var app = express();
var exec = require('child_process').exec;
var bodyParser = require('body-parser');
var jsonfile = require('jsonfile');


app.use(bodyParser.json()); // support json encoded bodies
app.use(bodyParser.urlencoded({ extended: true })); // support encoded bodies


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
    var rdata = req.body;
    var jfile = 'savedinput/'+Date.now()+'.json';
    var jfileout = 'savedoutput/'+Date.now()+'.json';
    jsonfile.writeFile(jfile, rdata, function (err, success) {
        if(err) {
            console.log(err);
        }
        else {


            exec('Rscript r/silly.r '+jfile+' '+jfileout, function(error, stdout, stderr) {
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
                    //res.sendfile(jfileout);
                    res.jsonp(JSON.stringify(jfileout));
                }
            });


        }
    });

    //res.jsonp('[1,2,3,4]');
});

app.listen(3000, function () {
    console.log('Example app listening on port 3000!');
});