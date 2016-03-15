import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['onezone-atlas'],
  didInsertElement() {
    this.$().parents().css('height', '100%');
  }
});
