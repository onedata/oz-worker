import Ember from 'ember';
import PageBase from './_page-base';
import UnauthenticatedRouteMixin from 'ember-simple-auth/mixins/unauthenticated-route-mixin';

let LoginRoute = PageBase.extend(UnauthenticatedRouteMixin);

export default LoginRoute.extend({
  server: Ember.inject.service('server'),

  name: 'login',

  actions: {
    authenticate(provider) {
      //this.get('session').authenticate('authenticator:basic');
       this.get('server').publicRPC('getLoginEndpoint', provider, (answer) => {
         window.location = answer;
       });
    }
  }
});
