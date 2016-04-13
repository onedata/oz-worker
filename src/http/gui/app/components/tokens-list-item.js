import Ember from 'ember';
import safeElementId from '../utils/safe-element-id';

/**
 * A token entry in tokens-list.
 * @module components/tokens-list-item
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  store: Ember.inject.service('store'),
  onezoneServer: Ember.inject.service('onezoneServer'),

  /** Should be injected */
  token: null,

  classNames: ['tokens-list-item'],

  clipboardTarget: function() {
    return `#${this.get('inputContainerId')} input`;
  }.property('inputContainerId'),

  inputContainerId: function() {
    if (this.get('token.id')) {
      return safeElementId(`clienttoken-input-${this.get('token.id')}`);
    } else {
      return null;
    }
  }.property('token', 'token.id'),

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
