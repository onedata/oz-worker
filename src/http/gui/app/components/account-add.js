import Ember from 'ember';
import bindFloater from '../utils/bind-floater';

/**
 * An add account button, which shows popup with authorization providers.
 * @module components/account-add
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  onezoneServer: Ember.inject.service('onezoneServer'),
  classNames: ['account-add', 'account-item'],
  authProviders: null,

  didInsertElement() {
    let popup = this.$().find('.account-add-popup');
    let updater = bindFloater(popup);
    this.$().on('mouseover', updater);
    $('.accordion-container').on('scroll', updater);
  },

  generateAuthProviders: function () {
    let authProviders = [];
    let allAuthProviders = {
      google: 'Google+',
      facebook: 'Facebook',
      github: 'GitHub',
      dropbox: 'Dropbox',
      plgrid: 'PLGrid OpenID'
    };

    this.get('onezoneServer').getSupportedAuthorizers().then((data) => {
      data.forEach((authorizerId) => {
        authProviders.push([authorizerId, allAuthProviders[authorizerId]]);
      });
      authProviders = authProviders.map((item) => {
        return {
          type: item[0],
          label: `Connect by ${item[1]}`
        };
      });
      this.set('authProviders', authProviders);
    });
  }.on('init'),

  actions: {
    connectNewAccount(providerName) {
      this.get('onezoneServer').getConnectAccountEndpoint(providerName).then((url) => {
        window.location = url;
      });
    }
  }
});
