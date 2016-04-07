import Ember from 'ember';

export default Ember.Route.extend({
  model() {
    return this.modelFor('onezone').providers;
  },

  actions: {
    selectProvider(provider) {
      if (provider) {
        if (provider.get('isSelected')) {
          provider.set('isSelected', false);
        } else {
          let providers = this.controllerFor(this.routeName).get('model');
          providers.forEach((p) => p.set('isSelected', false));
          provider.set('isSelected', true);
        }
      }
    },
    deselectProviders() {
      let providers = this.controllerFor(this.routeName).get('model');
      providers.forEach((p) => p.set('isSelected', false));
    }
  }
});
