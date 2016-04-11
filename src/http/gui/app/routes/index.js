import Ember from 'ember';

/**
 * Just redirect to home.index, as there is a main redirection.
 * @module routes/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend({
  afterModel() {
    this.transitionTo('home.index');
  }
});
