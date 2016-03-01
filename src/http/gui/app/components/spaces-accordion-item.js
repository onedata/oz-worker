import Ember from 'ember';

export default Ember.Component.extend({
  /** Should be injected */
  space: null,

  classNames: ['spaces-accordion-item'],

  providers: function() {
    return this.get('space').get('providers');
  }.property('space'),

  cachedSupportToken: null,

  // TODO: maybe this code should be moved somewhere else... (make more global)
  didInsertElement() {
    let floater = $('.floater');

    let changePos = function() {
      let offset = floater.parent().offset();
      let left = `${parseInt(offset.left) + floater.parent().width()}px`;
      let top = offset.top;
      floater.css({left: left, top: top});
    };

    floater.parent().on('mouseover', changePos);
    $('.accordion-container').on('scroll', changePos);
  }
});
