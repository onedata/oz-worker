import Ember from 'ember';
import ApplicationRouteMixin from 'ember-simple-auth/mixins/application-route-mixin';

let ApplicationRoute = Ember.Route.extend(ApplicationRouteMixin);

export default ApplicationRoute.extend({
  activate() {
    this.get('session').on('authenticationSucceeded', () => {
      window.alert('authentication succeeded!');
    });
    this.get('session').on('invalidationSucceeded', () => {
      window.alert('session has been invalidated!');
    });
  }
});
