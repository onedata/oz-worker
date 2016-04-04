import Ember from 'ember';

export default Ember.Component.extend({
  store: Ember.inject.service('store'),
  onezoneServer: Ember.inject.service(),

  classNames: ['secondary-accordion', 'alias-panel', 'accordion-content'],

  correctAlias: function() {
    return !!this.get('aliasText');
  }.property('aliasText'),

  aliasText: null,
  aliasTextEdit: null,

  aliasEditing: false,

  updateAliasText: function() {
    this.get('onezoneServer').getUserAlias().then(
      (alias) => {
        this.set('aliasText', alias);
      },
      (error) => {
        window.alert('Getting alias failed: ' + error);
      }
    );
  }.on('init'),

  actions: {
    startEditAlias: function() {
      if (!this.get('aliasEditing')) {
        this.set('aliasTextEdit', this.get('aliasText'));
        this.set('aliasEditing', true);
      }
    },

    // TODO: this should be invoked when pressing Esc when in editing mode
    // TODO: maybe create a global object, in which any cancel action can be registered
    // TODO: eg. service('cancel').register(fun);

    cancelEditAlias: function() {
      if (this.get('aliasEditing')) {
        this.set('aliasEditing', false);
      }
    },

    endEditAlias: function(aliasName) {
      try {
        this.get('onezoneServer').setUserAlias(aliasName).then(
          (newAlias) => {
            this.set('aliasText', newAlias);
            console.debug('Set alias successful');
          },
          (error) => {
            window.alert('Set alias failed: ' + error.message);
          }
        );
      } finally {
        this.set('aliasEditing', false);
      }
    }
  }
});
