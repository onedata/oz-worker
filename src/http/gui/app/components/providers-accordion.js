import Ember from 'ember';

// TODO: shared base class with spaces-accordion
export default Ember.Component.extend({
  classNames: ['providers-accordion', 'accordion-content'],

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
  }
});
