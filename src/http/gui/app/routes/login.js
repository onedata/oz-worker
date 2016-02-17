import PageBase from './_page-base';
import UnauthenticatedRouteMixin from 'ember-simple-auth/mixins/unauthenticated-route-mixin';

let LoginRoute = PageBase.extend(UnauthenticatedRouteMixin);

export default LoginRoute.extend({
  name: 'login',

  actions: {
    authenticate() {
      this.get('session')
        .authenticate('authenticator:basic').catch((reason) => {
          window.alert(`you shall NOT pass, because: ${reason}`);
        });
    }
  }
});
