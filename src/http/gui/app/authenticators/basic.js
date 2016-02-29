// jshint unused:false

import Ember from 'ember';

// see: http://ember-simple-auth.com/api/classes/BaseAuthenticator.html
import Base from 'ember-simple-auth/authenticators/base';

export default Base.extend({
  session: Ember.inject.service('session'),
  server: Ember.inject.service('server'),

  authenticate(options) {
    return new Ember.RSVP.Promise((resolve/*, reject*/) => {
      // Just resolve the promise - this is called from websocket handler
      // when the server has sent sessionDetails to the client, which means
      // it has a session.
      resolve();
    });
  },

  restore(data) {
    // Inform the websocket adapter that we anticipate session restoring.
    console.debug('auth restore start ');
    return this.get('server').tryToRestoreSession();
  },

  invalidate(data) {
    console.debug('auth invalidate start ');
    return new Ember.RSVP.Promise((resolve) => {
      // TODO
      this.get('session').set('opData', null);
      resolve();
    });
  }
});
