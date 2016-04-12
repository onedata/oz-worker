import Ember from 'ember';
import bindFloater from '../utils/bind-floater';

/**
 * A popup (drop) with fixed position placed near to the provider-place widget,
 * visible when clicked. Contains information about provider and its spaces.
 * Fixed position automatically updates on some events, like atlas resize.
 * @module components/provider-place-drop
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
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

  spaces: function() {
    return this.get('provider.spaces');
  }.property('provider', 'provider.spaces'),

  spacesSorting: ['isDefault:desc', 'name'],
  spacesSorted: Ember.computed.sort('spaces', 'spacesSorting'),

  /** If true, places provider drop on the left of provider place circle */
  dropSideLeft: function() {
    return this.get('provider.longitude') >= 0;
  }.property('provider.longitude'),

  /** Returns a class name */
  dropSide: function() {
    return this.get('dropSideLeft') ? 'drop-left' : 'drop-right';
  }.property('dropSideLeft'),

  /** Binds a fixed position update event */
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
