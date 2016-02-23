import Ember from 'ember';
import ApplicationRouteMixin from 'ember-simple-auth/mixins/application-route-mixin';

let ApplicationRoute = Ember.Route.extend(ApplicationRouteMixin);

export default ApplicationRoute.extend({
  activate() {
    // examples on registering additional session events handlers
    this.get('session').on('authenticationSucceeded', () => {
      console.debug('authentication succeeded!');
    });
    this.get('session').on('invalidationSucceeded', () => {
      console.debug('session has been invalidated!');
    });
  },



  // TODO: invoked only when application is reloaded!
  initSession: function() {
    console.debug('authentication moved to login route action');
  }.on('init')
});
