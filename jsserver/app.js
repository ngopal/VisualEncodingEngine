var express = require('express');
var app = express();
var exec = require('child_process').exec;

app.get('/', function (req, res) {
    res.sendfile("/www/index.html", {root: __dirname});
});

app.get('/sfunction', function (req, res) {
    exec('Rscript r/silly.r this is a test', function(error, stdout, stderr) {
        //res.send(stdout);
        if (error) { console.log(error); }
        else if (stderr) { console.log(stderr); }
        else if (stdout) {
            console.log("STDOUT");
            res.sendfile("savedoutput/test.json");
        }
    });
    //res.send('success!');
});

app.listen(3000, function () {
    console.log('Example app listening on port 3000!');
});