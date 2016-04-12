import Ember from 'ember';

// TODO: shared base class with spaces-accordion
/**
 * A container for providers-accordion-item - contains a list of providers.
 * @module components/providers-accordion
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  classNames: ['secondary-accordion', 'providers-accordion', 'accordion-content'],

  providers: null,
  providersSorting: ['isDefault:desc', 'name'],
  providersSorted: Ember.computed.sort('providers', 'providersSorting'),

  noProviders: function() {
    let providers = this.get('providers');
    return providers.get('length') === 0;
  }.property('providers.length'),

  didInsertElement() {
    // TODO: implement a search bar
    // this.$().find('#providers-list').searchable({
    //   searchField: '#providers-list-search',
    //   selector: '.list-group-item',
    //   childSelector: '.item',
    //   show: function( elem ) {
    //     elem.slideDown(100);
    //   },
    //   hide: function( elem ) {
    //     elem.slideUp( 100 );
    //   }
    // });
  },

  actions: {
    selectProvider(provider) {
      if (provider) {
        this.get('providers').forEach((p) => p.set('isSelected', false));
        provider.set('isSelected', true);
      }
    }
  }
});
