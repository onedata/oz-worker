import Ember from 'ember';

// see: http://ember-simple-auth.com/api/classes/BaseAuthenticator.html
import Base from 'ember-simple-auth/authenticators/base';

export default Base.extend({
  restore(data) {
    console.debug('auth restore');
    return new Ember.RSVP.Promise((resolve/*, reject*/) => {
      resolve();
    });
  },
  authenticate(options) {
    console.debug('auth authenticate');
    return new Ember.RSVP.Promise((resolve) => {
      resolve('hello');
    });
  },
  invalidate(data) {
    console.debug('auth invalidate');
    return new Ember.RSVP.Promise((resolve) => {
      resolve();
    });
  }
});
