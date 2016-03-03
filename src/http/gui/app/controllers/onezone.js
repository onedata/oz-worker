import Ember from 'ember';

export default Ember.Controller.extend({
  queryParams: ['expand_accounts'],
  expandAccounts: function() {
    return this.get('expand_accounts') === 'true';
  }.property('expand_accounts')
});
