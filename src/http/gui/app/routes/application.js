import Ember from 'ember';
import ApplicationRouteMixin from 'ember-simple-auth/mixins/application-route-mixin';

export default Ember.Route.extend(ApplicationRouteMixin, {
  session: Ember.inject.service('session'),

  // please use instance-initializers/session-events to handle session events
  // activate() {
    // examples on registering additional session events handlers
    // this.get('session').on('authenticationSucceeded', () => {
    //   console.debug('authentication succeeded!');
    // });
    // this.get('session').on('invalidationSucceeded', () => {
    //   console.debug('session has been invalidated!');
    // });
  // },

  initSession: function () {
    // @todo This returns a promise. We should display a loading page here
    // and transition to proper page on promise resolve.
    this.get('session').initSession().then(
      () => {
        console.log('initSession resolved');
      },
      // TODO: do not use window.alert
      () => {
        window.alert('Fatal error: session cannot be initialized');
      }
    );
  }.on('init')
});
