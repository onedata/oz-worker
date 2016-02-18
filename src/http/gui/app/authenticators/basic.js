// jshint unused:false

import Ember from 'ember';

// see: http://ember-simple-auth.com/api/classes/BaseAuthenticator.html
import Base from 'ember-simple-auth/authenticators/base';

export default Base.extend({
  server: Ember.inject.service('server'),
  authenticate(options) {
    console.debug('auth authenticate start');
    return new Ember.RSVP.Promise((resolve, reject) => {
      this.get('server').callServer('sessionDetails', (response) => {
        console.debug('auth server response: ' + JSON.stringify(response));
        if (response.sessionDetails) {
          resolve(response);
        } else {
          reject(response);
        }
      });
    });
  },
  restore(data) {
    console.debug('auth restore start');
    return new Ember.RSVP.Promise((resolve/*, reject*/) => {
      // TODO
      resolve();
    });
  },
  invalidate(data) {
    console.debug('auth invalidate start');
    return new Ember.RSVP.Promise((resolve) => {
      // TODO
      resolve();
    });
  }
});
