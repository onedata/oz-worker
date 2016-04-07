import Ember from 'ember';

// TODO: shared base class with spaces-accordion
export default Ember.Component.extend({
  classNames: ['secondary-accordion', 'providers-accordion', 'accordion-content'],

  noProviders: function() {
    let providers = this.get('providers');
    return providers.get('length') === 0;
  }.property('providers.length'),

  didInsertElement() {
    this.$().find('#providers-list').searchable({
      searchField: '#providers-list-search',
      selector: '.list-group-item',
      childSelector: '.item',
      show: function( elem ) {
        elem.slideDown(100);
      },
      hide: function( elem ) {
        elem.slideUp( 100 );
      }
    });
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
