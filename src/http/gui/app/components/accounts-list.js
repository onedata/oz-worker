import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['accounts-list', 'accordion-content', 'sidebar-list'],
  // TODO: set order of login providers
  // accountsSorted: function() {
  //   let authorizers = this.get('authorizers');
  //   return authorizers && authorizers.sort((a, b) => a.type < b.type);
  // }.property('authorizers')
  initAuth: function() {
  }.observes('authorizers')
});
