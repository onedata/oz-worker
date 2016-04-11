import Ember from 'ember';

// TODO: tests please, because it was a draft; integrate with atlas
/**
 * Conditionally displays a message for user instead of providers world map.
 * @module components/onezone-modal-container
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  session: Ember.inject.service('session'),

  /** Should be injected */
  providers: null,

  isFirstLogin: function() {
    let sessionDetails = this.get('session').get('sessionDetails');
    return sessionDetails && sessionDetails.firstLogin;
  }.property('session.sessionDetails'),

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
