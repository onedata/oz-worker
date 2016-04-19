import Ember from 'ember';

/**
 * Content of onezone application - a world map with providers.
 * @module routes/home/onezone/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
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
