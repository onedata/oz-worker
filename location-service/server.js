'use strict';

var bodyParser = require("body-parser");
var express = require('express');
var async = require('async');
var dns = require('dns');

var kad = require('kad');
var constants = require('kad/lib/constants');
var MemStore = require('kad-memstore');
var CouchbaseStore = require('./couchbase-store');
var Consistency = require('kad-consistency').ConsistentNode;
var extend = require('util')._extend;

// todo: include sybil/spartacus guards
//var spartacus = require('kad-spartacus');

var Node = Consistency(kad.Node);
var Contact = kad.contacts.AddressPortContact;

/**
 * Location Service server which provides REST service
 * and is connected to the kad network.
 *
 * @param options
 * @param options.bootstrapNodes - list of kad bootstrap nodes
 * @param options.verbosity      - verbosity level [0-4]
 * @param options.port           - port for kad service
 * @param options.restPort       - port for REST API
 * @param options.host           - host to start kad client and REST API at
 * @param options.couchbaseAddress  - address of couchbase (if omitted MemStore will be used)
 * @returns {Server}
 * @constructor
 */
function Server(options) {
  if (!(this instanceof Server)) {
    return new Server(options);
  }
  this.options = options;

  this.contact = Contact({
    address: this.options.host,
    port: this.options.port
  });
  var loggerID = this.options.host + ':' + this.options.port + ':' + this.contact.nodeID;
  this.logger = new kad.Logger(this.options.verbosity || 0, loggerID);
  var validator = function (key, value, callback) {
    callback(true);
  };
  this.transport = (this.options.transport || kad.transports.HTTP)(this.contact, {
    logger: this.logger
  });
  this.router = new kad.Router({
    transport: this.transport,
    validator: validator,
    logger: this.logger
  });

  var storage;
  if (this.options.couchbaseAddress) {
    storage = CouchbaseStore(this.options.couchbaseAddress);
    this.logger.info('Using Couchbase Store to store data at: ' + this.options.couchbaseAddress);
  } else {
    storage = MemStore();
    this.logger.warn('Using transient MemStore to store data');
  }

  this.node = new Node(extend(this.options, {
    transport: this.transport,
    router: this.router,
    logger: this.logger,
    storage: storage,
    validator: validator
  }));
}

/**
 * Starts kad client.
 * @param callback      - called upon successful client initialisation
 * @param exitCallback  - called when client lost connectivity to other peers
 */
Server.prototype.startDHT = function (callback, exitCallback) {
  var node = this.node;
  this._enableExitOnNotConnected(exitCallback);
  async.each(this.options.bootstrapNodes, function (nodeString, done) {
    var tokens = nodeString.split(":");
    var host = tokens[0];
    var port = parseInt(tokens[1], 10);

    dns.resolve(host, 'A', function (err, addresses) {
      if (err) node._log.warn(host + " unresolvable via DNS");

      addresses = addresses || [];
      addresses.push(host);
      addresses = Array.from(new Set(addresses));

      node._log.info(host + " resolved to " + addresses);
      async.each(addresses, function (ip, done) {
        var contact = Contact({address: host, port: port});
        node.connect(contact, done)
      });
      done()
    })
  }, callback);
};

/**
 * Starts REST API listener.
 */
Server.prototype.startREST = function () {
  var server = this;
  this.app = express();
  this.app.use(bodyParser.urlencoded({extended: false}));
  this.app.use(bodyParser.json());
  this.app.get('/:id', function (request, response) {
    server.node.get(request.params.id, function (err, value) {
      response.send(server._getResultAsRestResponse(err, value));
    })
  });
  this.app.post('/:id', function (request, response) {
    server.node.put(request.params.id, request.body.value, function (err) {
      response.send(server._getResultAsRestResponse(err));
    })
  });
  this.app.listen(this.options.restPort);
};

Server.prototype._enableExitOnNotConnected = function (exitCallback) {
  var router = this.router;
  var logger = this.logger;
  exitCallback = exitCallback || logger.error;

  var handleChange = function () {
    if (router.length === 0) {
      logger.error("Empty routing table");
      exitCallback()
    }
  };
  router.on('shift', handleChange);
};

/**
 * Converts kad operation result to REST response object.
 * @param err   - kad operation error
 * @param value - actual kad operation result
 * @returns {object}
 */
Server.prototype._getResultAsRestResponse = function (err, value) {
  var result = {};
  result.api_version = '0.1';

  if (err) {
    result.message = err.message || ('Unknown error: ' + JSON.stringify(err));
    result.status = err.code || 'UNKNOWN_ERROR'
  } else {
    result.status = 'OK'
  }

  if (value) {
    result.value = value
  }
  return result
};

module.exports = Server;