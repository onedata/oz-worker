import Ember from 'ember';
import AuthenticatedRouteMixin from 'ember-simple-auth/mixins/authenticated-route-mixin';

let LogoutRoute = Ember.Route.extend(AuthenticatedRouteMixin);

/**
 * Try to invalidate session in server, then call backend logout.
 * @module routes/home/logout
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default LogoutRoute.extend({
  activate() {
    console.debug('logout activate ');
    // TODO: real error handling
    this.get('session').invalidate().then(
      () => {
        console.debug('logout promise ');
        window.location = '/do_logout';
      },
      () => {
        // TODO: do not use window.alert - use modal instead
        window.alert('Session invalidation failed - cannot logout');
      }
    );
  }
});
