import Ember from 'ember';
import AuthenticatedRouteMixin from 'ember-simple-auth/mixins/authenticated-route-mixin';

let LogoutRoute = Ember.Route.extend(AuthenticatedRouteMixin);

export default LogoutRoute.extend({
  activate() {
    console.debug('logout activate ');
    this.get('session').invalidate().then(()=> {
      console.debug('logout promise ');
      window.location = '/do_logout';
    });
    //Ember.$.ajax({ url: '/logout.html', type: 'POST' });
  }
});
