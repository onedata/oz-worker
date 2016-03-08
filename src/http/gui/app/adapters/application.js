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
let FIND = 'find';
let FIND_ALL = 'findAll';
let FIND_QUERY = 'findQuery';
let FIND_MANY = 'findMany';
let FIND_HAS_MANY = 'findHasMany';
let FIND_BELONGS_TO = 'findBelongsTo';
let CREATE_RECORD = 'createRecord';
let UPDATE_RECORD = 'updateRecord';
let DELETE_RECORD = 'deleteRecord';
let FETCH_RESP = 'fetchResp';
let FETCH_REQ = 'fetchReq';

let RPC_REQ = 'RPCReq';
let RPC_RESP = 'RPCResp';


//let PULL_RESULT = "result";
//let MSG_TYPE_PUSH_UPDATED = "pushUpdated";
//let MSG_TYPE_PUSH_DELETED = "pushDeleted";

export default DS.RESTAdapter.extend({
  initialized: false,
  onOpenCallback: null,
  onErrorCallback: null,

  // Promises that will be resolved when response comes
  promises: new Map(),
  // The WebSocket
  socket: null,
  // Queue of messages before the socket is open
  beforeOpenQueue: [],

  /** -------------------------------------------------------------------
   * WebSocket initialization
   * ------------------------------------------------------------------- */

  /** Called automatically on adapter init. */
  init() {
    this.initializeWebSocket();
  },

  /** Initializes the WebSocket */
  initializeWebSocket(onOpen, onError) {
    // Register callbacks even if WebSocket is already being initialized.
    if (onOpen) {
      this.set('onOpenCallback', onOpen);
    }
    if (onError) {
      this.set('onErrorCallback', onError);
    }
    if (this.get('initialized') === false) {
      this.set('initialized', true);
      let adapter = this;

      let protocol = window.location.protocol === 'https:' ? 'wss://' : 'ws://';
      //let querystring = window.location.pathname + window.location.search;
      let host = window.location.hostname;
      let port = window.location.port;

      let url = protocol + host + (port === '' ? '' : ':' + port) + '/ws/'; // + querystring;
      console.log('Connecting: ' + url);

      if (adapter.socket === null) {
        adapter.socket = new WebSocket(url);
        adapter.socket.onopen = function (event) {
          adapter.open.apply(adapter, [event]);
        };
        adapter.socket.onmessage = function (event) {
          adapter.receive.apply(adapter, [event]);
        };
        adapter.socket.onerror = function (event) {
          adapter.error.apply(adapter, [event]);
        };
      }
    }
  },

  /** -------------------------------------------------------------------
   * Adapter API
   * ------------------------------------------------------------------- */

  /** Indicates if consecutive calls to findAll should use cached records or
   * fetch all again. */
  shouldReloadAll() {
    return true;
  },

  /** Developer function - for logging/debugging */
  logToConsole(fun_name, fun_params) {
    console.log(fun_name + '(');
    if (fun_params) {
      for (let i = 0; i < fun_params.length; i++) {
        console.log('    ' + String(fun_params[i]));
      }
    }
    console.log(')');
  },

  /** Called when ember store wants to find a record */
  find(store, type, id, record) {
    this.logToConsole(FIND, [store, type, id, record]);
    return this.asyncRequest(FIND, type.modelName, id);
  },

  /** Called when ember store wants to find all records of a type */
  findAll(store, type, sinceToken) {
    this.logToConsole(FIND_ALL, [store, type, sinceToken]);
    return this.asyncRequest(FIND_ALL, type.modelName, null, sinceToken);
  },

  /** Called when ember store wants to find all records that match a query */
  findQuery(store, type, query) {
    this.logToConsole(FIND_QUERY, [store, type, query]);
    return this.asyncRequest(FIND_QUERY, type.modelName, null, query);
  },

  /** Called when ember store wants to find multiple records by id */
  findMany(store, type, ids, records) {
    this.logToConsole(FIND_MANY, [store, type, ids, records]);
    return this.asyncRequest(FIND_MANY, type.modelName, null, ids);
  },

  /** @todo is this needed? **/
  findHasMany(store, record, url, relationship) {
    this.logToConsole(FIND_HAS_MANY, [store, record, url, relationship]);
    return 'not_implemented';
  },

  /** @todo is this needed? */
  findBelongsTo(store, record, url, relationship) {
    this.logToConsole(FIND_BELONGS_TO, [store, record, url, relationship]);
    return 'not_implemented';
  },

  /** Called when ember store wants to create a record */
  createRecord(store, type, record) {
    this.logToConsole(CREATE_RECORD, [store, type, record]);
    let data = {};
    let serializer = store.serializerFor(type.modelName);
    serializer.serializeIntoHash(data, type, record, {includeId: true});
    return this.asyncRequest(CREATE_RECORD, type.modelName, null, data);
  },

  /** Called when ember store wants to update a record */
  updateRecord(store, type, record) {
    this.logToConsole(UPDATE_RECORD, [store, type, record]);
    let data = {};
    let serializer = store.serializerFor(type.modelName);
    serializer.serializeIntoHash(data, type, record, {includeId: true});
    let id = Ember.get(record, 'id');
    return this.asyncRequest(UPDATE_RECORD, type.modelName, id, data);
  },

  /** Called when ember store wants to delete a record */
  deleteRecord(store, type, record) {
    this.logToConsole(DELETE_RECORD, [store, type, record]);
    let id = Ember.get(record, 'id');
    return this.asyncRequest(DELETE_RECORD, type.modelName, id);
  },

  /** @todo is this needed? */
  groupRecordsForFindMany(store, records) {
    this.logToConsole('groupRecordsForFindMany', [store, records]);
    return [records];
  },

  /** -------------------------------------------------------------------
   * RPC API
   * ------------------------------------------------------------------- */

  /**
   * Calls back to the server. Useful for getting information like
   * user name etc. from the server or performing some operations that
   * are not model-based.
   * @param {string} type - identifier of resource, e.g. 'public' for public RPC
   * @param {string} operation - function identifier
   * @param {object} data - json data
   */
  RPC(type, operation, data) {
    this.logToConsole('RPC', [type, operation, data]);
    let payload = {
      msgType: RPC_REQ,
      resourceType: type,
      operation: operation,
      data: data
    };
    return this.sendAndRegisterPromise(operation, type, payload);
  },

  /** -------------------------------------------------------------------
   * Internal functions
   * ------------------------------------------------------------------- */

  /**
   * Performs an sync request to server side and stores a handle to the
   * promise, which will be resolved in receive function.
   */
  asyncRequest(operation, type, ids, data) {
    this.logToConsole('asyncRequest', [operation, type, ids, data]);
    if (!ids) {
      ids = null;
    }
    if (!data) {
      data = null;
    }
    let payload = {
      msgType: FETCH_REQ,
      resourceType: type,
      operation: operation,
      resourceIds: ids,
      data: this.transformRequest(data, type, operation)
    };
    return this.sendAndRegisterPromise(operation, type, payload);
  },

  /**
   * Sends a payload (JSON) via WebSocket, previously adding a randomly
   * generated UUID to it and registers a promise
   * (which can later be retrieved by the UUID).
   */
  sendAndRegisterPromise(operation, type, payload) {
    // Add UUID to payload so we can later connect the response with a promise
    // (the server will include this uuid in the response)
    let uuid = this.generateUuid();
    payload.uuid = uuid;
    let adapter = this;
    return new Ember.RSVP.Promise(function (resolve, reject) {
      let success = function (json) {
        Ember.run(null, resolve, json);
      };
      let error = function (json) {
        Ember.run(null, reject, json);
      };
      adapter.promises.set(uuid, {
        success: success,
        error: error,
        type: type,
        operation: operation
      });
      console.log('registerPromise: ' + JSON.stringify(payload));
      adapter.send(payload);
    });
  },

  /** Generates a random uuid */
  generateUuid() {
    let date = new Date().getTime();
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g,
      function (character) {
        let random = (date + Math.random() * 16) % 16 | 0;
        date = Math.floor(date / 16);
        return (character === 'x' ? random : (random & 0x7 | 0x8)).toString(16);
      });
  },

  /**
   * Used to transform some types of requests, because they carry different
   * information than Ember assumes.
   */
  transformRequest(json, type, operation) {
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
   * Transform response received from WebSocket to the format expected
   * by Ember.
   */
  transformResponse(json, type, operation) {
    let result = {};
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

  /** WebSocket onopen callback */
  open() {
    let onOpen = this.get('onOpenCallback');
    if (onOpen) {
      onOpen();
    }
    // Send messages waiting in queue
    if (this.beforeOpenQueue.length > 0) {
      this.beforeOpenQueue.forEach(function (payload) {
        this.socket.send(JSON.stringify(payload));
      });
      this.beforeOpenQueue = [];
    }
  },

  /** Used to send a payload (JSON) through WebSocket. If the WS is not
   * established yet, it will put the payload in a queue, which will be
   * sent when connection is on. */
  send(payload) {
    if (this.socket.readyState === 1) {
      this.socket.send(JSON.stringify(payload));
    }
    else {
      this.beforeOpenQueue.push(payload);
    }
  },

  /** WebSocket onmessage callback, resolves promises with received replies. */
  receive(event) {
    let adapter = this;
    let promise;
    console.log('received: ' + event.data);
    let json = JSON.parse(event.data);
    // Received a response to data fetch
    if (json.msgType === FETCH_RESP) {
      promise = adapter.promises.get(json.uuid);
      if (json.result === 'ok') {
        // TODO VFS-1508: sometimes, the callback is undefined - debug
        let transformed_data = adapter.transformResponse(json.data,
          promise.type, promise.operation);
        console.log('FETCH_RESP success: ' + JSON.stringify(transformed_data));

        promise.success(transformed_data);
      } else {
        console.log('FETCH_RESP error: ' + json.data);
        promise.error(json.data);
      }
      // TODO @todo implement on generic data type
      //} else if (json.msgType == MSG_TYPE_PUSH_UPDATED) {
      //    App.File.store.pushPayload('file', {
      //        file: json.data
      //    })
      //} else if (json.msgType == MSG_TYPE_PUSH_DELETED) {
      //    let record_id;
      //    for (record_id in json.data) {
      //        App.File.store.find('file', record_id).then(function (post) {
      //            App.File.store.unloadRecord(post);
      //        });
      //    }
    } else if (json.msgType === RPC_RESP) {
      promise = adapter.promises.get(json.uuid);
      if (json.result === 'ok') {
        console.log('RPC_RESP success: ' + JSON.stringify(json.data));
        promise.success(json.data);
      } else {
        console.log('RPC_RESP error: ' + JSON.stringify(json.data));
        promise.error(json.data);
      }
    }
    adapter.promises.delete(json.uuid);
  },

  /** WebSocket onerror callback */
  error(event) {
    let adapter = this;
    // TODO @todo better error handling
    // window.alert('WebSocket error, see console for details.');
    console.error(`WebSocket connection error, event data: ` + event.data);

    let onError = this.get('onErrorCallback');
    if (onError) {
      onError();
    }

    adapter.promises.forEach(function (promise) {
      console.log('promise.error -> ' + promise);
      promise.error();
    });
    adapter.promises.clear();
  }
});
