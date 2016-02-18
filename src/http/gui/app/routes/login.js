import PageBase from './_page-base';
import UnauthenticatedRouteMixin from 'ember-simple-auth/mixins/unauthenticated-route-mixin';

let LoginRoute = PageBase.extend(UnauthenticatedRouteMixin);

export default LoginRoute.extend({
  name: 'login',

  actions: {
    authenticate() {
      window.alert('authentication moved to application route init');
    }
  }
});
