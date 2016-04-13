import Ember from 'ember';

/**
 * Provides API for backend methods designed for Onezone.
 * @module services/onezone-server
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Service.extend({
  server: Ember.inject.service(),

  /**
   Fetch supported authorizers and return it in the promise resolve as a
   list of strings
   @returns {RSVP.Promise}
  */
  getSupportedAuthorizers() {
    return this.get('server').publicRPC('getSupportedAuthorizers');
  },

  /**
    Fetch token for provider and return it in the promise resolve
    @param {string} spaceId
    @returns {RSVP.Promise}
  */
  getSupportToken(spaceId) {
    return this.get('server').privateRPC('getSupportToken', {spaceId: spaceId});
  },

  /**
    Fetch URL to provider and return it in the promise resolve.
    @param {string} providerId
    @returns {RSVP.Promise}
  */
  getProviderRedirectURL(providerId) {
    return this.get('server').privateRPC('getRedirectURL', {providerId: providerId});
  },

  /**
    Fetch URL to login endpoint.
    @param {string} providerName One of login providers, eg. google, dropbox
    @returns {RSVP.Promise}
  */
  getLoginEndpoint(providerName) {
    return this.get('server').publicRPC('getLoginEndpoint', {provider: providerName});
  },

  /**
    Fetch URL to authenticator endpoint.
    - resolves with url (string)
    @param {string} providerName One of login providers, eg. google, dropbox
    @returns {RSVP.Promise}
  */
  getConnectAccountEndpoint(providerName) {
    return this.get('server').privateRPC('getConnectAccountEndpoint', {provider: providerName});
  },

  /**
   Fetch zone name.
   - resolves with name (string)
   - rejects with no value on error
   @returns {RSVP.Promise}
  */
  getZoneName() {
    return this.get('server').publicRPC('getZoneName');
  },

  /**
   Fetch user alias stored in backend.
   - resolves with alias (string)
   - rejects with error (string)
   @returns {RSVP.Promise}
  */
  getUserAlias() {
    return this.get('server').privateRPC('getUserAlias');
  },

  /**
   Set new user alias.
   - resolves with no value
   - rejects with error (string)
   @returns {RSVP.Promise}
  */
  setUserAlias(userAlias) {
    return this.get('server').privateRPC('setUserAlias', {
      userAlias: userAlias
    });
  }
});
