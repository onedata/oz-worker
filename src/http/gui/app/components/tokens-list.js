import Ember from 'ember';

// TODO: shared base class with spaces-accordion
export default Ember.Component.extend({
  store: Ember.inject.service('store'),
  classNames: ['tokens-accordion', 'accordion-content'],

  actions: {
    createNewToken: function() {
      // TODO: id is mocked now - it will be returned from server on save
      let token = this.get('store').createRecord('clienttoken', {});
      token.save();
    },
  }
});
