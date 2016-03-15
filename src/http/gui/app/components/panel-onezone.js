import Ember from 'ember';

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
