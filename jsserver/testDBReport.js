var MongoClient = require('mongodb').MongoClient
    , assert = require('assert');


// MongoDB functions and parameter definitions
var mongourl = 'mongodb://localhost:27017/pilot';
MongoClient.connect(mongourl, function(dberr, db) {
        assert.equal(null, dberr);
        console.log("Connected succesfully to server");
        db.collection('evaldata').find({}).count(function(e,count){
          console.log(e, count)
        })

        db.close()
    });






