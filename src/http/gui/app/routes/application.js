import Ember from 'ember';
import ApplicationRouteMixin from 'ember-simple-auth/mixins/application-route-mixin';

let ApplicationRoute = Ember.Route.extend(ApplicationRouteMixin);

export default ApplicationRoute.extend({
  activate() {
    // examples on registering session events handlers
    this.get('session').on('authenticationSucceeded', () => {
      console.debug('authentication succeeded!');
    });
    this.get('session').on('invalidationSucceeded', () => {
      console.debug('session has been invalidated!');
    });
  },

  // TODO: invoked only when application is reloaded!
  initSession: function() {
    this.get('session')
      .authenticate('authenticator:basic').then(
        () => {
          let data = this.get('session.data.authenticated');
          console.debug('app route - init sesssion: ' + JSON.stringify(data));
        },
        (reason) => {
          console.warn(`you shall NOT pass, because: ${reason}`);
        }
      );
  }.on('init')
});
