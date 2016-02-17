import Ember from 'ember';

// see: http://ember-simple-auth.com/api/classes/BaseAuthenticator.html
import Base from 'ember-simple-auth/authenticators/base';

export default Base.extend({
  restore(data) {
    window.alert('auth restore');
    return new Ember.RSVP.Promise((resolve/*, reject*/) => {
      resolve();
    });
  },
  authenticate(options) {
    window.alert('auth authenticate');
    return new Ember.RSVP.Promise((resolve) => {
      resolve('hello');
    });
  },
  invalidate(data) {
    window.alert('auth invalidate');
    return new Ember.RSVP.Promise((resolve) => {
      resolve();
    });
  }
});
