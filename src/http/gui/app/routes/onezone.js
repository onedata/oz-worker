import Ember from 'ember';
// import AuthenticatedRouteMixin from 'ember-simple-auth/mixins/authenticated-route-mixin';

// let OnezoneRoute = Ember.Route.extend(AuthenticatedRouteMixin);
let OnezoneRoute = Ember.Route.extend({});

export default OnezoneRoute.extend({
  model() {
    let data = {
      providers: this.store.findAll('provider'),
      spaces: this.store.findAll('space'),
      authorizers: this.store.findAll('authorizer')
    };
    return data;
  }
});
