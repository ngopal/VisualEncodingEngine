var MongoClient = require('mongodb').MongoClient
  , assert = require('assert');

// Connection URL
var url = 'mongodb://localhost:27017/';

//// Use connect method to connect to the server
//MongoClient.connect(url, function(err, db) {
//  assert.equal(null, err);
//  console.log("Connected succesfully to server");
//
//  insertDocuments(db, function() {
//    findDocuments(db, function() {
//      db.close();
//    });
//  });
//});
//
//var insertDocuments = function(db, callback) {
//  // Get the documents collection
//  var collection = db.collection('documents');
//  // Insert some documents
//  collection.insertMany([
//    {a : 1}, {a : 2}, {a : 3}
//  ], function(err, result) {
//    assert.equal(err, null);
//    assert.equal(3, result.result.n);
//    assert.equal(3, result.ops.length);
//    console.log("Inserted 3 documents into the collection");
//    callback(result);
//  });
//}
//
//var findDocuments = function(db, callback) {
//  // Get the documents collection
//  var collection = db.collection('documents');
//  // Find some documents
//  collection.find({}).toArray(function(err, docs) {
//    assert.equal(err, null);
//    console.log("Found the following records");
//    console.log(docs)
//    callback(docs);
//  });
//}


function insertOneItem(database, user, page, time, dataObject) {
  var collection = database.collection('evaldata');
  var user = user;
  var page = page;
  collection.insertOne({user : user, page : page, time : time, dataObj : dataObject}, function(insertErr, insertResult) {
    assert.equal(insertErr, null);
    assert(1, insertResult.result.n);
    assert(1, insertResult.ops.length);
  });
}

function findAll(database, callback) {
  var collection = database.collection('evaldata');
  collection.find({}).toArray(function(err, docs) {
    assert.equal(err, null);
    console.log("Found following:")
    //console.log(docs);
    // For more depth and detail, uncomment below
    var numDocs = docs.length;
    for (var i = 0; i < numDocs; i++) {
      var numObj = docs[0].dataObj.length;
      for (var j = 0; j < numObj; j++) {
        console.log(docs[i].dataObj[j]);
      }
    }
  })
}

function findOneItemOnPage(database, page) {
  var collection = database.collection('evaldata');
  collection.find({page:page}).toArray(function(err, docs) {
    assert.equal(err, null);
    console.log("Found following:");
    console.log(docs);
  })
}

function findOneItemOnTime(database, time) {
  var collection = database.collection('evaldata');
  collection.find({time:time}).toArray(function(err, docs) {
    assert.equal(err, null);
    console.log("Found following:")
    console.log(docs);
  })
}

MongoClient.connect(url, function(err, db) {
  assert.equal(null, err);
  console.log("Connected succesfully to server");

  //insertOneItem(db, 'kaskdjaslkd', '30', Date.now(), {color:'red'});
  //findOneItemOnPage(db, '30');
  findAll(db);
  db.close();
});
