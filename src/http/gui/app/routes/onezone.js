import Ember from 'ember';
import AuthenticatedRouteMixin from 'ember-simple-auth/mixins/authenticated-route-mixin';

//let OnezoneRoute = Ember.Route.extend(AuthenticatedRouteMixin);
//let OnezoneRoute = Ember.Route.extend({});

export default Ember.Route.extend(AuthenticatedRouteMixin, {
  model() {
    return {
      providers: this.store.findAll('provider'),
      spaces: this.store.findAll('space'),
      authorizers: this.store.findAll('authorizer')
    }
  }
});
