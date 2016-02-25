import Ember from 'ember';

export default Ember.Component.extend({
  /** Should be injected */
  space: null,

  classNames: ['spaces-accordion-item'],

  providersCount: function() {
    return this.get('providers').get('length');
  }.property('space'),

  providers: function() {
    return this.get('space').get('providers');
  }.property('space')
});
