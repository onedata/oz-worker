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

  /**
   * Informs the websocket adapter that a session restoring is anticipated,
   * and when sessionDetails come from the server, the promise is resolved
   * rather than new authentication is performed.
   */
  tryToRestoreSession: function () {
    return this.get('store').adapterFor('application').tryToRestoreSession();
  },

  /**
   * Sends an RPC call to the server for a publicly available resource.
   * thenFun is evaluated on response from the server.
   */
  publicRPC: function (operation, data, thenFun) {
    this.get('store').adapterFor('application')
      .callback('public', operation, data).then(thenFun);
  },

  /**
   * Sends an RPC call to the server for a resource that is restricted to
   * logged in clients.
   * thenFun is evaluated on response from the server.
   */
  privateRPC: function (operation, data, thenFun) {
    this.get('store').adapterFor('application')
      .callback('private', operation, data).then(thenFun);
  }
});
