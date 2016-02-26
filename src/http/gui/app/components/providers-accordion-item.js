import Ember from 'ember';

export default Ember.Component.extend({
  /** Should be injected */
  provider: null,

  iconName: function() {
    let provider = this.get('provider');
    return (provider && provider.get('isDefault')) ? 'provider-default' : 'provider';
  }.property('provider'),

  iconColor: function() {
    let provider = this.get('provider');
    return (provider && provider.get('isWorking')) ? 'provider-working' : 'provider-not-working';
  }.property('provider'),

  classNames: ['providers-accordion-item'],

  spaces: function() {
    return this.get('provider').get('spaces');
  }.property('provider')
});
