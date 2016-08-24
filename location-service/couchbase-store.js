'use strict';

var couchbase = require('couchbase');
var ReadableStream = require('readable-stream');

module.exports = CouchbaseStore;

function CouchbaseStore(address) {
  if (!(this instanceof CouchbaseStore)) {
    return new CouchbaseStore(address);
  }

  var cluster = new couchbase.Cluster(address);
  this.bucket = cluster.openBucket('location_service');
  this._initView();

}

CouchbaseStore.prototype.put = function (key, value, callback) {
  this.bucket.upsert(key, JSON.parse(value), callback)
};

CouchbaseStore.prototype.get = function (key, callback) {
  this.bucket.get(key, function (err, res) {
    if (res && res.value) {
      callback(err, JSON.stringify(res.value))
    } else {
      callback(new Error('Not found in Couchbase'))
    }
  })
};

CouchbaseStore.prototype.del = function (key, callback) {
  this.bucket.remove(key, callback)
};

CouchbaseStore.prototype.createReadStream = function () {
  var store = this;
  var values = null;
  var i = 0;

  return new ReadableStream({
    objectMode: true,
    read: function () {
      var stream = this;

      if (values === null) {
        var query = couchbase.ViewQuery.from('location_service_design', 'locations');
        store.bucket.query(query, function (err, res) {
          values = res;
        });
      }

      function doPush() {
        if (values === null) return setTimeout(doPush, 50);
        if (i >= values.length) {
          stream.push(null);
        } else {
          setImmediate(function pushItem() {
            var reserved = i;
            i++;
            stream.push({key: values[reserved].id, value: values[reserved].value});
          });
        }
      }

      doPush()
    }
  });
};

CouchbaseStore.prototype._initView = function () {
  var ddocdata = {
    views: {
      locations: {
        map: 'function(doc, meta) { emit(doc.name, doc); }'
      }
    }
  };
  this.bucket.manager().upsertDesignDocument('location_service_design', ddocdata, function (err) {
    if (err != null) {
      console.error("Could not (re)create view due to: ", err);
      process.exit(-2)
    }
  });
};
