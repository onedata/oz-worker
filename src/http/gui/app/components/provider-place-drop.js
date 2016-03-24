import Ember from 'ember';
import bindFloater from '../utils/bind-floater';

export default Ember.Component.extend({
  onezoneServer: Ember.inject.service('onezone-server'),
  classNames: ['provider-place-drop'],
  classNameBindings: ['isWorking', 'dropSide'],

  isWorking: function() {
    return this.get('provider.isWorking') ? 'working' : '';
  }.property('provider.isWorking'),

  /** Parent component - must be injected! */
  providerPlace: null,

  provider: function() {
    return this.get('providerPlace.provider');
  }.property('providerPlace'),

  dropSideLeft: function() {
    return this.get('provider.longitude') >= 0;
  }.property('provider.longitude'),

  dropSide: function() {
    return this.get('dropSideLeft') ? 'drop-left' : 'drop-right';
  }.property('dropSideLeft'),

  didInsertElement() {
    let popup = this.$();
    let updater = bindFloater(popup, null, {
      posX: (this.get('dropSideLeft') ? 'left' : 'right'),
      posY: 'middle',
      offsetY: 12,
      offsetX: 16 * (this.get('dropSideLeft') ? -1 : 1),
    });
    this.$().on('mouseover', updater);
    this.$().parent().on('mouseover', updater);
    $(window).resize(updater);
    $(window).scroll(updater);
  },

  actions: {
    goToFiles() {
      this.get('onezoneServer').getProviderRedirectURL(this.get('provider.id')).then((url) => {
        window.location = url;
      });
    }
  }
});
