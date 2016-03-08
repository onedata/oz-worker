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
   * Forces the WebSocket adapter to initialize a WebSocket connection.
   * Can register onOpen, onError callbacks that will be called after
   * the connection is established or refused.
   */
  initializeWebSocket: function (onOpen, onError) {
    this.get('adapter').initializeWebSocket(onOpen, onError);
  },

  /**
   * Sends a RPC call via WebSocket asking for session data, i.e. if the session
   * is valid and session details such as user name.
   * Returns a promise that will be called with received data.
   */
  sessionRPC: function () {
    return this.get('adapter').RPC('session');
  },

  /**
   * Sends an RPC call to the server for a publicly available resource.
   * Returns a promise that will be called with received data.
   */
  publicRPC: function (operation, data) {
    return this.get('adapter').RPC('public', operation, data);
  },

  /**
   * Sends an RPC call to the server for a resource that is restricted to
   * logged in clients.
   * Returns a promise that will be called with received data.
   */
  privateRPC: function (operation, data) {
    return this.get('adapter').RPC('private', operation, data);
  }
});
