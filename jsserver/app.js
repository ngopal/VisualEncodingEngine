var express = require('express');
var app = express();
var exec = require('child_process').exec;

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

app.listen(3000, function () {
    console.log('Example app listening on port 3000!');
});