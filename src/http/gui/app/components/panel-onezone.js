import Ember from 'ember';

/**
 * A message panel for onezone-modal-container - show a message instead of
 * providers world map.
 * @module
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  classNames: ['panel', 'panel-onezone'],
  classNameBindings: ['panelType'],

  // null or 'alert'
  type: null,

  /** If true, text from title will be used as i18n key */
  useI18n: false,

  evalTitle: function() {
    if (this.get('useI18n')) {
      return this.get('i18n').t(this.get('title'));
    } else {
      return this.get('title');
    }
  }.property('title', 'useI18n'),

  panelType: function() {
    let type = this.get('type');
    return type ? `panel-onezone-${type}` : '';
  }.property('type')
});
