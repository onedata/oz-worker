import Ember from 'ember';

/**
 * A navbar toggle button ("hamburger"), which can be bot
 * @module components/navbar-toggle
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  clickHandlers: [],

  /** If enabled and menu is collapsed - show "open menu" button */
  enableOpenMenuButton: true,

  /** If enabled and menu is uncollapsed (toggled) - show "close menu" button */
  enableCloseMenuButton: true,

  classNameBindings: ['top-right-fixed'],
  'top-right-fixed': function() {
    return this.get('topRight');
  }.property('topRight'),

  /**
   * Add a function to invoke when top menu button is clicked
   * The handler will be invoked with one argument: instance of this component.
   */
  registerClickHandler(handler) {
    this.set('clickHandlers', this.get('clickHandlers').concat([handler]));
  },

  didInsertElement() {
    // register click handlers - this is a helper method, not used now
    this.$().find('button.navbar-toggle').click(() => {
      this.get('clickHandlers').forEach((handler) => { handler(this); });
    });

    this.$().find('button.navbar-toggle').bindFirst('click', () => {
      if (this.$('button.navbar-toggle').attr('aria-expanded') === 'true') {
        $('.global-fog').removeClass('active');
      } else {
        $('.global-fog').addClass('active');
      }
    });
  }
});
