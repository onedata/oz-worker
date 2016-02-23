import Ember from 'ember';

export default Ember.Route.extend({
  topMenuService: Ember.inject.service('top-menu'),

  activate() {
    this.get('topMenuService').selectItem(this.get('name'));
  }
});
