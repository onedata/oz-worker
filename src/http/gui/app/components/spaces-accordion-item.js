import Ember from 'ember';

export default Ember.Component.extend({
  server: Ember.inject.service('server'),

  /** Should be injected */
  space: null,

  classNames: ['spaces-accordion-item'],

  providers: function() {
    return this.get('space').get('providers');
  }.property('space'),

  supportToken: null,

  // TODO: maybe this code should be moved somewhere else... (make more global)
  didInsertElement() {
    $('.floater').each(function() {
      let ft = $(this);

      let changePos = function() {
        let offset = ft.parent().offset();
        let left = `${parseInt(offset.left) + ft.parent().width()}px`;
        let top = offset.top;
        ft.css({left: left, top: top});
      };

      ft.parent().on('mouseover', changePos);
      $('.accordion-container').on('scroll', changePos);
    });
  },

  actions: {
      getNewSupportToken: function() {
        let space = this.get('space');
        if (space) {
          this.get('server').getSupportToken(space.id, (token) => {
            // TODO: only debug, should be removed in future
            console.debug('Fetched new support token: ' + token);
            this.set('supportToken', token);
          });
        } else {
          console.warn('Tried to get new support token, but no space is assigned to item');
        }
      },
  }
});
