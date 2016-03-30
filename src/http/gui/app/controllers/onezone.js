import Ember from 'ember';

export default Ember.Controller.extend({
  init: function () {
    this._super();
    Ember.run.schedule('afterRender',this,function() {
      this.send('expandQuerySpecifiedAccordions');
    });
  },

  queryParams: ['expand_accounts', 'expand_spaces', 'expand_providers', 'expand_tokens'],

  expandAccounts: function() {
    return this.get('expand_accounts') === 'true';
  }.property('expand_accounts'),

  expandSpaces: function() {
    return this.get('expand_spaces') === 'true';
  }.property('expand_spaces'),

  expandProviders: function() {
    return this.get('expand_providers') === 'true';
  }.property('expand_providers'),

  expandTokens: function() {
    return this.get('expand_tokens') === 'true';
  }.property('expand_tokens'),

  actions: {
    expandQuerySpecifiedAccordions: function() {
      if (this.get('expandAccounts')) {
        $('#collapse-accounts').collapse('show');
      }
      if (this.get('expandSpaces')) {
        $('#collapse-spaces').collapse('show');
      }
      if (this.get('expandProviders')) {
        $('#collapse-providers').collapse('show');
      }
      if (this.get('expandTokens')) {
        $('#collapse-tokens').collapse('show');
      }
    }
  },
});
