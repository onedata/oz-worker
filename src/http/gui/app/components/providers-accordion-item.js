import Ember from 'ember';

export default Ember.Component.extend({
  /** Should be injected */
  provider: null,

  classNames: ['providers-accordion-item'],

  spaces: function() {
    return this.get('provider').get('spaces');
  }.property('provider')
});
