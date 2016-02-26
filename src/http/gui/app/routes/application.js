import Ember from 'ember';
//import ApplicationRouteMixin from 'ember-simple-auth/mixins/application-route-mixin';

//let ApplicationRoute = Ember.Route.extend(ApplicationRouteMixin);

export default Ember.Route.extend({
  store: Ember.inject.service('store'),

  activate() {
    // examples on registering additional session events handlers
    this.get('session').on('authenticationSucceeded', () => {
      console.debug('authentication succeeded!');
    });
    this.get('session').on('invalidationSucceeded', () => {
      console.debug('session has been invalidated!');
    });
  },

  initAdapter: function () {
    this.get('store').adapterFor('application').init();
  }.on('init')
});
