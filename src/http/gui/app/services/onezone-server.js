import Ember from 'ember';

export default Ember.Service.extend({
  server: Ember.inject.service('server'),

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
    Fetch URL to provider and return it in the promise resolve
    @param {string} providerId
    @returns {RSVP.Promise}
  */
  getProviderRedirectURL(providerId) {
    return this.get('server').privateRPC('getRedirectURL', {providerId: providerId});
  },

  /**
    Fetch URL to authenticator endpoint
    @param {string} providerName One of login providers, eg. google, dropbox
    @returns {RSVP.Promise}
  */
  getLoginEndpoint(providerName) {
    return this.get('server').publicRPC('getLoginEndpoint', {provider: providerName});
  },

  /**
    Fetch URL to authenticator endpoint
    @param {string} providerName One of login providers, eg. google, dropbox
    @returns {RSVP.Promise}
  */
  getConnectAccountEndpoint(providerName) {
    return this.get('server').privateRPC('getConnectAccountEndpoint', {provider: providerName});
  },

  getZoneName() {
    return this.get('server').publicRPC('getZoneName');
  },

  getUserAlias() {
    return this.get('server').privateRPC('getUserAlias');
  },

  setUserAlias(userAlias) {
    return this.get('server').privateRPC('setUserAlias', {
      userAlias: userAlias
    });
  }
});
