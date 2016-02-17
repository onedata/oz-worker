import Ember from 'ember';

export default Ember.Component.extend({
  clickHandlers: [],

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
