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
   * Sends a callback to the server. thenFun is evaluated on response from
   * the server.
   */
  callServer: function (key, thenFun) {
    this.get('store').adapterFor('application').callback('global', key).then(thenFun);
  }
});
