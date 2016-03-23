import Ember from 'ember';
import bindFloater from '../utils/bind-floater';

export default Ember.Component.extend({
  classNames: ['provider-place-drop', 'drop-left'],

  /** Parent component - must be injected! */
  providerPlace: null,

  provider: function() {
    return this.get('providerPlace.provider');
  }.property('providerPlace'),

  didInsertElement() {
    let popup = this.$();
    let updater = bindFloater(popup, null, {
      posX: 'left',
      posY: 'middle',
      offsetY: 12,
      offsetX: -16,
    });
    this.$().on('mouseover', updater);
    this.$().parent().on('mouseover', updater);
    $(window).resize(updater);
  }
});
