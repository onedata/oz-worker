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
  }

  //actions: {
  //  authenticate(providerName) {
  //    this.get('onezoneServer').getLoginEndpoint(providerName).then((url) => {
  //      window.location = url;
  //    });
  //  }
  //}
});
