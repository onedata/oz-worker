import Ember from 'ember';
import AuthenticatedRouteMixin from 'ember-simple-auth/mixins/authenticated-route-mixin';

export default Ember.Route.extend(AuthenticatedRouteMixin, {
  model() {
    return {
      providers: this.store.findAll('provider'),
      spaces: this.store.findAll('space'),
      authorizers: this.store.findAll('authorizer'),
      clienttokens: this.store.findAll('clienttoken')
    };
  }
});
