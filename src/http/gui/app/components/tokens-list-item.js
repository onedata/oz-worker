import Ember from 'ember';
export default Ember.Component.extend({
  store: Ember.inject.service('store'),
  onezoneServer: Ember.inject.service('onezoneServer'),

  /** Should be injected */
  token: null,

  classNames: ['tokens-list-item'],

  clipboardTarget: function() {
    return `#${this.get('inputId')}`;
  }.property('inputId'),

  inputId: function() {
    return `clienttoken-input-${this.get('token.id')}`;
  }.property('token.id'),

  actions: {
    remove() {
      this.get('token').destroyRecord();
    },

    selectTokenText() {
      let input = this.$().find('input')[0];
      input.setSelectionRange(0, input.value.length);
    },

    copySuccess() {
      console.debug('Token copied successfully');
    },

    copyError() {
      console.error('Token copy error');
    }
  }
});
