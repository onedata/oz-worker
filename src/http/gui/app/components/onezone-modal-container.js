import Ember from 'ember';

export default Ember.Component.extend({
  session: Ember.inject.service('session'),

  /** Should be injected */
  providers: null,

  isFirstLogin: function() {
    let sessionDetails = this.get('session').get('opData').sessionDetails;
    return sessionDetails && sessionDetails.firstLogin;
  }.property('session.opData'),

  /** If true, show "none providers" modal */
  modalNoneProviders: function() {
    return !this.get('isFirstLogin') &&
      (this.get('providers').filterBy('isWorking', true).length === 0);
  }.property('providers.@each.isWorking', 'isFirstLogin'),

  modalFirstLogin: function() {
    return this.get('isFirstLogin');
  }.property('isFirstLogin'),

  modalGetSupport: function() {
    return !this.get('isFirstLogin') &&
      (this.get('providers').get('length') === 0);
  }.property('providers', 'isFirstLogin')
});
