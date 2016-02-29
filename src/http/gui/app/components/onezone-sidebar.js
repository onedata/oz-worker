import Ember from 'ember';

export default Ember.Component.extend({
  /** Providers list should be injected */
  providers: null,
  /** Spaces list should be injected */
  spaces: null,
  /** AuthAccounts list should be injected */
  authAccounts: null,

  onChange: function() {
    let a = this.get('authAccounts');
    console.log(a);
  }.observes('authAccounts')
});
