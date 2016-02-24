import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['accounts-list', 'accordion-content'],
  accounts: [
    {
      type: 'google',
      label: 'lukasz.opiola@gmail.com'
    },
    {
      type: 'google',
      label: 'opiola.lukasz.2@gmail.com'
    },
    {
      type: 'plgrid',
      label: 'plgopiola'
    }
  ],
  // TODO: set order of login providers
  accountsSorted: function() {
    return this.get('accounts').sort((a, b) => a.type < b.type);
  }.property('accounts')
});
