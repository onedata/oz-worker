import Ember from 'ember';
import bindFloater from '../utils/bind-floater';
import safeElementId from '../utils/safe-element-id';

/**
 * Provider entry in sidebar. Contains list of its spaces.
 * @module components/providers-accordion-item
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  store: Ember.inject.service('store'),
  onezoneServer: Ember.inject.service('onezoneServer'),

  /** Provider model to display - should be injected */
  provider: null,

  spaces: Ember.computed('provider.spaces', function() {
    return this.get('provider.spaces');
  }),
  spacesSorting: ['isDefault:desc', 'name'],
  spacesSorted: Ember.computed.sort('spaces', 'spacesSorting'),

  collapseId: function() {
    return safeElementId(`collapse-provider-${this.get('provider.id')}`);
  }.property('provider', 'provider.id'),

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
    /** Currently set its provider selection */
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
