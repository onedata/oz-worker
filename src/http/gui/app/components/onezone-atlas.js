import Ember from 'ember';

/**
 * A world map, on which providers are placed.
 * @module components/onezone-atlas
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  classNames: ['onezone-atlas'],

  /** Atlas image aspect ratio - needed when recomputing new atlas size */
  ATLAS_AR: 2.0347372134038797,

  /** Resizes a atlas to fit its container, keeping aspect ratio */
  resizeToFit: function() {
    let element = this.$();
    let parent = element.parent();
    let parentWidth = parent.width();
    let parentHeight = parent.height();
    let newWidth;
    let newHeight;
    newWidth = parentWidth;
    newHeight = newWidth * (1/this.ATLAS_AR);

    if (newHeight > parentHeight) {
      newHeight = parentHeight;
      newWidth = newHeight * this.ATLAS_AR;
    }

    this.set('width', newWidth);
    this.set('height', newHeight);
  },

  widthChanged: function() {
    this.$().width(this.get('width'));
  }.observes('width'),

  heightChanged: function() {
    this.$().height(this.get('height'));
  }.observes('height'),

  centerX: function() {
    return this.get('width')/2;
  }.property('width'),

  centerY: function() {
    return this.get('height')/2;
  }.property('height'),

  didInsertElement() {
    this.$().parents().css('height', '100%');
    this.resizeToFit();
    $(window).resize(() => this.resizeToFit());
  },

  actions: {
    deselectProviders() {
      this.sendAction('deselectProviders');
    }
  }
});
