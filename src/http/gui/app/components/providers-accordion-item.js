import Ember from 'ember';
import bindFloater from '../utils/bind-floater';

export default Ember.Component.extend({
  store: Ember.inject.service('store'),
  server: Ember.inject.service('server'),

  /** Should be injected */
  provider: null,

  iconName: function() {
    let provider = this.get('provider');
    return (provider && provider.get('isDefault')) ? 'provider-default' : 'provider';
  }.property('provider.isDefault'),

  iconColor: function() {
    let provider = this.get('provider');
    return (provider && provider.get('isWorking')) ? 'provider-working' : 'provider-not-working';
  }.property('provider.isWorking'),

  classNames: ['providers-accordion-item'],

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
    /** Redirects to OneProvider using url from provider model */
    goToProvider() {
      let provider = this.get('provider');
      if (provider) {
        this.get('server').getProviderRedirectURL(provider.get('id')).then((url) => {
          window.location = url;
        });
      }
    },

    /** Set the provider as default, unsetting other providers */
    toggleDefault() {
      let store = this.get('store');
      // TODO: use query?
      // there should be only one default provider, but for safety...
      let defaultProviders = store.peekAll('provider').filterBy('isDefault', true);
      defaultProviders.toArray().forEach((p) => {
        p.set('isDefault', false);
        p.save();
      });

      let provider = this.get('provider');
      provider.set('isDefault', true);
      provider.save();
    }
  }
});
