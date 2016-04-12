import Ember from 'ember';
import AuthenticatedRouteMixin from 'ember-simple-auth/mixins/authenticated-route-mixin';

/**
 * Main entry to onezone application - load neccessary data for application.
 * @module routes/onezone
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend(AuthenticatedRouteMixin, {
  model() {
    return {
      providers: this.store.findAll('provider'),
      spaces: this.store.findAll('space'),
      authorizers: this.store.findAll('authorizer'),
      clienttokens: this.store.findAll('clienttoken')
    };
  }
});
