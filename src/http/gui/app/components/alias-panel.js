import Ember from 'ember';

/**
 * One of main sidebar items: allows to change alias.
 * @module components/alias-panel
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  store: Ember.inject.service(),
  onezoneServer: Ember.inject.service(),

  classNames: ['secondary-accordion', 'alias-panel', 'accordion-content'],

  /** Returns true if aliasText is not blank - used for showing no alias message otherwise */
  correctAlias: function() {
    return !!this.get('aliasText');
  }.property('aliasText'),

  /** Client-side known alias */
  aliasText: null,

  /** Client-side temporary value of alias edit field */
  aliasTextEdit: null,

  /** True if in alias edit mode (shows alias edit) */
  aliasEditing: false,

  /** Fetch alias from server on init - sets aliasText */
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
