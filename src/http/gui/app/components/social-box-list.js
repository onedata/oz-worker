import Ember from 'ember';

/**
 * Renders buttons for supported login providers. A container for social-boxes.
 * @module components/social-box-list
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  onezoneServer: Ember.inject.service(),

  /**
   * Object with support mapping, eg. ``{plgrid: true, facebook: false}``
   * If authorizer is supported, its button will be displayed.
   * Allowed supporters: plgrid, dropbox, facebook, google
   *
   * Set by initSupportedAuthorizers
   */
  supportedAuthorizers: null,

  /** Fetches list of supported authorizers from server; sets supportedAuthorizers */
  initSupportedAuthorizers: function () {
    this.get('onezoneServer').getSupportedAuthorizers().then((data) => {
      let authorizers = {};
      data.forEach((authorizerId) => {
        authorizers[authorizerId] = true;
      });
      this.set('supportedAuthorizers', authorizers);
    });
  }.on('init'),

  actions: {
    // TODO: what if there is server error?
    /** Get a login endpoint URL from server and go to it */
    authenticate(providerName) {
      this.$().find(`.login-icon-box.${providerName}`).addClass('active');
      this.get('onezoneServer').getLoginEndpoint(providerName).then(
        (url) => {
          window.location = url;
        },
        (error) => {
          // TODO: use modal instead of window.alert
          window.alert('Getting authentication endpoint failed: ' + error);
          this.$().find(`.login-icon-box.${providerName}`).removeClass('active');
        }
      );
    }
  }
});
