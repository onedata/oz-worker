import Ember from 'ember';
import AuthenticatedRouteMixin from 'ember-simple-auth/mixins/authenticated-route-mixin';

let OnezoneRoute = Ember.Route.extend(AuthenticatedRouteMixin);

export default OnezoneRoute.extend({
  model() {
    return {
      providers: this.store.findAll('provider'),
      spaces: this.store.findAll('space')
    };
  }
});
