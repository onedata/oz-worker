import Ember from 'ember';
import PageBase from './_page-base';
import UnauthenticatedRouteMixin from 'ember-simple-auth/mixins/unauthenticated-route-mixin';

let LoginRoute = PageBase.extend(UnauthenticatedRouteMixin);

export default LoginRoute.extend({
  onezoneServer: Ember.inject.service('onezoneServer'),
  name: 'login',

  beforeModel() {
    if (this.get('session.isAuthenticated')) {
      this.transitionTo('onezone');
    }
  },

  model() {
    return new Ember.RSVP.Promise((resolve/*, reject*/) => {
      this.get('onezoneServer').getZoneName().then(
        (zoneName) => {
          resolve({
            zoneName: zoneName
          });
        },
        () => {
          console.error('Failed to get zone name');
          resolve({
            zoneName: null,
          });
        }
      );
    });
  }

  //actions: {
  //  authenticate(providerName) {
  //    this.get('onezoneServer').getLoginEndpoint(providerName).then((url) => {
  //      window.location = url;
  //    });
  //  }
  //}
});
