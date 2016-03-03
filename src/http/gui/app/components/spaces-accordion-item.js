import Ember from 'ember';
import bindFloater from '../utils/bind-floater';

export default Ember.Component.extend({
  store: Ember.inject.service('store'),
  server: Ember.inject.service('server'),

  /** Should be injected */
  space: null,

  classNames: ['spaces-accordion-item'],

  supportToken: null,

  didInsertElement() {
    this.$().find('.floater').each(function() {
      let ft = $(this);
      let updatePosition = bindFloater(ft);
      ft.parent().on('mouseover', updatePosition);
      // TODO: performance - better all updatePositions in one fun
      $('.accordion-container').on('scroll', updatePosition);
    });
  },

  actions: {
      // TODO: this action should not be invoked when there is currently opened other token
      getNewSupportToken: function() {
        let space = this.get('space');
        if (space) {
          this.get('server').getSupportToken(space.get('id')).then((token) => {
            // TODO: only debug, should be removed in future
            console.debug('Fetched new support token: ' + token);
            this.set('supportToken', token);
          });
        } else {
          console.warn('Tried to get new support token, but no space is assigned to item');
        }
      },

      /** Set the space as default, unsetting other spaces */
      toggleDefault() {
        let store = this.get('store');
        // TODO: use query?
        // there should be only one default provider, but for safety...
        let defaultSpaces = store.peekAll('space').filterBy('isDefault', true);
        defaultSpaces.toArray().forEach((p) => {
          p.set('isDefault', false);
          p.save();
        });

        let space = this.get('space');
        space.set('isDefault', true);
        space.save();
      }
  }
});
