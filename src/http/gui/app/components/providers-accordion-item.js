import Ember from 'ember';
import bindFloater from '../utils/bind-floater';

export default Ember.Component.extend({
  store: Ember.inject.service('store'),
  onezoneServer: Ember.inject.service('onezoneServer'),

  /** Should be injected */
  provider: null,

  iconName: function() {
    let provider = this.get('provider');
    return (provider && provider.get('isDefault')) ? 'provider-home' : 'provider';
  }.property('provider.isDefault'),

  iconColor: function() {
    let provider = this.get('provider');
    return (provider && provider.get('isWorking')) ? 'provider-working' : 'provider-not-working';
  }.property('provider.isWorking'),

  classNames: ['secondary-accordion-item', 'providers-accordion-item'],

  didInsertElement() {
    this.$().find('.floater').each(function() {
      let ft = $(this);
      let updatePosition = bindFloater(ft);
      ft.parent().on('mouseover', updatePosition);
      // TODO: performance - better all updatePositions in one fun
      $('.accordion-container').on('scroll', updatePosition);
    });
  },

  actions: {
    // Old behaviour - redirects directly to Oneprovider using url
    // /** Redirects to OneProvider using url from provider model */
    // goToProvider() {
    //   let provider = this.get('provider');
    //   if (provider) {
    //     this.get('onezoneServer').getProviderRedirectURL(provider.get('id')).then((url) => {
    //       window.location = url;
    //     });
    //   }
    // },

    goToProvider() {
      let provider = this.get('provider');
      this.sendAction('selectProvider', provider);
    },

    /** Set or unset the provider as default (can unset other providers) */
    toggleDefault() {
      let store = this.get('store');
      let provider = this.get('provider');
      if (provider.get('isDefault')) {
        provider.set('isDefault', false);
      } else {
        // TODO: use query?
        // there should be only one default provider, but for safety...
        let defaultProviders = store.peekAll('provider').filterBy('isDefault', true);
        defaultProviders.toArray().forEach((p) => {
          p.set('isDefault', false);
          p.save();
        });
        provider.set('isDefault', true);
      }
      provider.save();
    }
  }
});
