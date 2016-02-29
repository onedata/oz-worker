/**
 * A default adapter of application - Oneprovide WebSocket
 * Custom adapter that handles model synchronization between client and server
 * using a websocket connection.
 * @module adapters/base/websocket-oneprovider
 * @author Łukasz Opioła
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import DS from 'ember-data';

// Interface between WebSocket Adapter client and server. Corresponding
// interface is located in gui_ws_handler.erl.
var FIND = 'find';
var FIND_ALL = 'findAll';
var FIND_QUERY = 'findQuery';
var FIND_MANY = 'findMany';
var FIND_HAS_MANY = 'findHasMany';
var FIND_BELONGS_TO = 'findBelongsTo';
var CREATE_RECORD = 'createRecord';
var UPDATE_RECORD = 'updateRecord';
var DELETE_RECORD = 'deleteRecord';

var PULL_RESP = 'pullResp';
var PULL_REQ = 'pullReq';
var CALLBACK_REQ = 'callbackReq';
var CALLBACK_RESP = 'callbackResp';
var SESSION_RESP = 'sessionResp';
var NO_SESSION_RESP = 'noSessionResp';
//var PULL_RESULT = "result";
//var MSG_TYPE_PUSH_UPDATED = "pushUpdated";
//var MSG_TYPE_PUSH_DELETED = "pushDeleted";

export default DS.RESTAdapter.extend({
  session: Ember.inject.service('session'),
  //
  sessionRestoreResolve: null,
  sessionRestoreReject: null,

  // Promises that will be resolved when response comes
  promises: new Map(),
  // The WebSocket
  socket: null,
  // Queue of messages before the socket is open
  beforeOpenQueue: [],


  /** If this is called, session data from websocket will resolve session
   * restoration rather than run authenticate. */
  tryToRestoreSession: function () {
    return new Ember.RSVP.Promise((resolve, reject) => {
      // This promise will be resolved whenever sessionDetails are sent
      // from the server over websocket.
      this.set('sessionRestoreResolve', resolve);
      this.set('sessionRestoreReject', reject);
    });
  },


  /** Initialize connection */
  init: function () {
    this.initializeSocket();
  },

  /** Developer function - for logging/debugging */
  logToConsole: function (fun_name, fun_params) {
    console.log(fun_name + '(');
    for (var i = 0; i < fun_params.length; i++) {
      console.log('    ' + String(fun_params[i]));
    }
    console.log(')');
  },

  /** Called when ember store wants to find a record */
  find: function (store, type, id, record) {
    this.logToConsole(FIND, [store, type, id, record]);
    return this.asyncRequest(FIND, type.modelName, id);
  },

  /** Called when ember store wants to find all records of a type */
  findAll: function (store, type, sinceToken) {
    this.logToConsole(FIND_ALL, [store, type, sinceToken]);
    return this.asyncRequest(FIND_ALL, type.modelName, null, sinceToken);
  },

  /** Called when ember store wants to find all records that match a query */
  findQuery: function (store, type, query) {
    this.logToConsole(FIND_QUERY, [store, type, query]);
    return this.asyncRequest(FIND_QUERY, type.modelName, null, query);
  },

  /** Called when ember store wants to find multiple records by id */
  findMany: function (store, type, ids, records) {
    this.logToConsole(FIND_MANY, [store, type, ids, records]);
    return this.asyncRequest(FIND_MANY, type.modelName, null, ids);
  },

  /** @todo is this needed? **/
  findHasMany: function (store, record, url, relationship) {
    this.logToConsole(FIND_HAS_MANY, [store, record, url, relationship]);
    return 'not_implemented';
  },

  /** @todo is this needed? */
  findBelongsTo: function (store, record, url, relationship) {
    this.logToConsole(FIND_BELONGS_TO, [store, record, url, relationship]);
    return 'not_implemented';
  },

  /** Called when ember store wants to create a record */
  createRecord: function (store, type, record) {
    this.logToConsole(CREATE_RECORD, [store, type, record]);
    var data = {};
    var serializer = store.serializerFor(type.modelName);
    serializer.serializeIntoHash(data, type, record, {includeId: true});
    return this.asyncRequest(CREATE_RECORD, type.modelName, null, data);
  },

  /** Called when ember store wants to update a record */
  updateRecord: function (store, type, record) {
    this.logToConsole(UPDATE_RECORD, [store, type, record]);
    var data = {};
    var serializer = store.serializerFor(type.modelName);
    serializer.serializeIntoHash(data, type, record, {includeId: true});
    var id = Ember.get(record, 'id');
    return this.asyncRequest(UPDATE_RECORD, type.modelName, id, data);
  },

  /** Called when ember store wants to delete a record */
  deleteRecord: function (store, type, record) {
    this.logToConsole(DELETE_RECORD, [store, type, record]);
    var id = Ember.get(record, 'id');
    return this.asyncRequest(DELETE_RECORD, type.modelName, id);
  },

  /** @todo is this needed? */
  groupRecordsForFindMany: function (store, records) {
    this.logToConsole('groupRecordsForFindMany', [store, records]);
    return [records];
  },

  /**
   * Used to transform some types of requests, because they carry different
   * information.
   */
  transformRequest: function (json, type, operation) {
    switch (operation) {
      case UPDATE_RECORD:
        return json[type];

      case CREATE_RECORD:
        return json[type];

      default:
        return json;
    }
  },

  /**
   * Transform response received from WebScoket to the format expected
   * by ember.
   */
  transformResponse: function (json, type, operation) {
    var result = {};
    switch (operation) {
      case FIND:
        result[type] = json;
        return result;

      case FIND_ALL:
        result[type] = json;
        return result;

      case FIND_QUERY:
        result[type] = json;
        return result;

      case FIND_MANY:
        result[type] = json;
        return result;

      case CREATE_RECORD:
        result[type] = json;
        return result;

      default:
        return json;
    }
  },

  /**
   * Performs an sync request to server side and stores a handle to the
   * promise, which will be resolved in message function.
   */
  asyncRequest: function (operation, type, ids, data) {
    var adapter = this;
    adapter.logToConsole('asyncRequest', [operation, type, ids, data]);
    var uuid = adapter.generateUuid();
    if (!ids) {
      ids = null;
    }
    if (!data) {
      data = null;
    }

    return new Ember.RSVP.Promise(function (resolve, reject) {
      var success = function (json) {
        Ember.run(null, resolve, json);
      };
      var error = function (json) {
        Ember.run(null, reject, json);
      };
      adapter.promises.set(uuid, {
        success: success,
        error: error,
        type: type,
        operation: operation
      });

      var payload = {
        msgType: PULL_REQ,
        uuid: uuid,
        resourceType: type,
        operation: operation,
        resourceIds: ids,
        data: adapter.transformRequest(data, type, operation)
      };

      console.log('JSON payload: ' + JSON.stringify(payload));
      if (adapter.socket.readyState === 1) {
        adapter.socket.send(JSON.stringify(payload));
      }
      else {
        adapter.beforeOpenQueue.push(payload);
      }
    });
  },

  /** Generates a random uuid */
  generateUuid: function () {
    var date = new Date().getTime();
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function (character) {
      var random = (date + Math.random() * 16) % 16 | 0;
      date = Math.floor(date / 16);
      return (character === 'x' ? random : (random & 0x7 | 0x8)).toString(16);
    });
  },

  /** Initializes the WebScoket */
  initializeSocket: function () {
    var adapter = this;

    var protocol = window.location.protocol === 'https:' ? 'wss://' : 'ws://';
    //var querystring = window.location.pathname + window.location.search;
    var host = window.location.hostname;
    var port = window.location.port;

    var url = protocol + host + (port === '' ? '' : ':' + port) + '/ws/'; // + querystring;
    console.log('Connecting: ' + url);

    if (adapter.socket === null) {
      adapter.socket = new WebSocket(url);
      adapter.socket.onopen = function (event) {
        adapter.open.apply(adapter, [event]);
      };
      adapter.socket.onmessage = function (event) {
        adapter.message.apply(adapter, [event]);
      };
      adapter.socket.onerror = function (event) {
        adapter.error.apply(adapter, [event]);
      };
    }
  },

  /** WebScoket onopen callback */
  open: function () {
    var adapter = this;

    if (adapter.beforeOpenQueue.length > 0) {
      adapter.beforeOpenQueue.forEach(function (payload) {
        adapter.socket.send(JSON.stringify(payload));
      });
      adapter.beforeOpenQueue = [];
    }
  },

  /** WebScoket onmessage callback, resolves promises with received replies. */
  message: function (event) {
    var adapter = this;
    var callback;
    console.log('received: ' + event.data);
    var json = JSON.parse(event.data);
    if (json.msgType === PULL_RESP) {
      if (json.result === 'ok') {
        callback = adapter.promises.get(json.uuid);
        console.log('success: ' + json.data);
        // TODO VFS-1508: sometimes, the callback is undefined - debug
        var transformed_data = adapter.transformResponse(json.data,
          callback.type, callback.operation);
        callback.success(transformed_data);
      } else {
        console.log('error: ' + json.data);
        adapter.promises.get(json.uuid).error(json.data);
      }
      adapter.promises.delete(json.uuid);
      // TODO @todo implement on generic data type
      //} else if (json.msgType == MSG_TYPE_PUSH_UPDATED) {
      //    App.File.store.pushPayload('file', {
      //        file: json.data
      //    })
      //} else if (json.msgType == MSG_TYPE_PUSH_DELETED) {
      //    var record_id;
      //    for (record_id in json.data) {
      //        App.File.store.find('file', record_id).then(function (post) {
      //            App.File.store.unloadRecord(post);
      //        });
      //    }
    } else if (json.msgType === CALLBACK_RESP) {
      callback = adapter.promises.get(json.uuid);
      callback.success(json.data);
    } else if (json.msgType === NO_SESSION_RESP) {
      // Reject session restoration as the websocket connection has no session
      let rejectFunction = this.get('sessionRestoreReject');
      if (rejectFunction) {
        console.log("SESSION REJECTED");
        rejectFunction();
        this.set('sessionRestoreResolve', null);
        this.set('sessionRestoreReject', null);
      }
    } else if (json.msgType === SESSION_RESP) {
      console.log(json.data);
      let resolveFunction = this.get('sessionRestoreResolve');
      if (resolveFunction) {
        console.log("SESSION RESTORED");
        resolveFunction();
        this.set('sessionRestoreResolve', null);
        this.set('sessionRestoreReject', null);
      } else {
        console.log("SESSION CREATED");
        this.get('session').authenticate('authenticator:basic');
      }
      this.get('session').set('opData', json.data);
    }
  },

  /** WebSocket onerror callback */
  error: function (event) {
    var adapter = this;
    // TODO @todo better error handling
    // window.alert('WebSocket error, see console for details.');
    console.error(`WebSocket connection error, event data: ` + event.data);

    // Reject session restoration as no websocket connection is active
    let rejectFunction = this.get('sessionRestoreReject');
    if (rejectFunction) {
      console.log("SESSION REJECTED");
      rejectFunction();
      this.set('sessionRestoreResolve', null);
      this.set('sessionRestoreReject', null);
    }

    adapter.promises.forEach(function (promise) {
      console.log('promise.error -> ' + promise);
      promise.error();
    });
    adapter.promises.clear();
  },

  /**
   * Calls back to the server. Useful for getting information like
   * user name etc. from the server or performing some operation that
   * are not model-based.
   * @param {string} type - an identifier of resource, e.g. 'global' for global data
   * @param {string} operation - function identifier
   * @param {object} data - json data
   */
  callback: function (type, operation, data) {
    var adapter = this;
    adapter.logToConsole('callback', [type, operation, data]);
    var uuid = adapter.generateUuid();

    return new Ember.RSVP.Promise(function (resolve, reject) {
      var success = function (json) {
        Ember.run(null, resolve, json);
      };
      var error = function (json) {
        Ember.run(null, reject, json);
      };
      adapter.promises.set(uuid, {
        success: success,
        error: error,
        type: type,
        operation: operation
      });

      var payload = {
        msgType: CALLBACK_REQ,
        uuid: uuid,
        resourceType: type,
        operation: operation,
        data: data
      };

      console.log('JSON payload: ' + JSON.stringify(payload));
      if (adapter.socket.readyState === 1) {
        adapter.socket.send(JSON.stringify(payload));
      }
      else {
        adapter.beforeOpenQueue.push(payload);
      }
    });
  }

});
