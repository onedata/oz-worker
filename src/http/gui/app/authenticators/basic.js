// jshint unused:false

import Ember from 'ember';

import Base from 'ember-simple-auth/authenticators/base';

export default Base.extend({
  session: Ember.inject.service('session'),

  authenticate(options) {
    console.debug('auth authenticate start ');
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
    return this.get('session').tryToRestoreSession();
  },

  invalidate(data) {
    console.debug('auth invalidate start ');
    return new Ember.RSVP.Promise((resolve) => {
      this.get('session').set('sessionDetails', null);
      resolve();
    });
  }
});
