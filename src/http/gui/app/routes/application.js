import Ember from 'ember';
import ApplicationRouteMixin from 'ember-simple-auth/mixins/application-route-mixin';

//let ApplicationRoute = Ember.Route.extend(ApplicationRouteMixin);

export default Ember.Route.extend(ApplicationRouteMixin, {
  server: Ember.inject.service('server'),
  session: Ember.inject.service('session'),

  activate() {
    // examples on registering additional session events handlers
    this.get('session').on('authenticationSucceeded', () => {
      console.debug('authentication succeeded!');
    });
    this.get('session').on('invalidationSucceeded', () => {
      console.debug('session has been invalidated!');
    });
  },

  initSession: function () {
    // @todo This returns a promise. We should display a loading page here
    // and transition to proper page on promise resolve.
    this.get('server').initWebSocketAndSession().then(
      () => {
        console.log('initWebSocketAndSession resolved');
      }
    );
  }.on('init')
});
