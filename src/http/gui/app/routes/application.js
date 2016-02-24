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


  //// TODO: invoked only when application is reloaded!
  //initSession: function () {
  //  let authenticated = this.get('session').get('isAuthenticated');
  //  if (!authenticated) {
  //    this.get('session')
  //      .authenticate('authenticator:basic').then(
  //      () => {
  //        let data = this.get('session.data.authenticated');
  //        console.debug('app route - init sesssion: ' + JSON.stringify(data));
  //      },
  //      (reason) => {
  //        console.warn(`you shall NOT pass, because: ${reason}`);
  //      }
  //    );
  //  }
  //}.on('init')
});
