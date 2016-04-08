import Ember from 'ember';

export default Ember.Component.extend({
  onezoneServer: Ember.inject.service('onezoneServer'),
  supportedAuthorizers: null,

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
    authenticate(providerName) {
      this.$().find(`.login-icon-box.${providerName}`).addClass('active');
      this.get('onezoneServer').getLoginEndpoint(providerName).then((url) => {
        window.location = url;
      });
    }
  }
});
