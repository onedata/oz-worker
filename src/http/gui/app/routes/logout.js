import Ember from 'ember';
import AuthenticatedRouteMixin from 'ember-simple-auth/mixins/authenticated-route-mixin';

let LogoutRoute = Ember.Route.extend(AuthenticatedRouteMixin);

export default LogoutRoute.extend({
  activate() {
    this.get('session').invalidate();
    window.location = '/do_logout';
    //Ember.$.ajax({ url: '/logout.html', type: 'POST' });
  }
});
