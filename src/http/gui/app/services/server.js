/**
 * Provides communication with Server.
 * @module services/server
 * @author Łukasz Opioła
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

export default Ember.Service.extend({
  store: Ember.inject.service('store'),
  adapter: function () {
    return this.get('store').adapterFor('application');
  }.property(),

  /**
   * Returns a promise that will be resolved only when a websocket connection
   * is successfully created and the server responds with session details.
   * When this is called in application router init, it ensures that
   * WS connection and session are active before the page renders
   */
  initWebSocketAndSession: function () {
    return this.get('adapter').initWebSocketAndSession();
  },

  tryToRestoreSession: function () {
    return this.get('adapter').tryToRestoreSession();
  },

  /**
   * Sends an RPC call to the server for a publicly available resource.
   * onSuccess is evaluated on response from the server.
   */

  publicRPC: function (operation, data) {
    return this.get('adapter').RPC('public', operation, data);
  },

  /**
   * Sends an RPC call to the server for a resource that is restricted to
   * logged in clients.
   * onSuccess is evaluated on response from the server.
   */
  privateRPC: function (operation, data) {
    return this.get('adapter').RPC('private', operation, data);
  }
});
