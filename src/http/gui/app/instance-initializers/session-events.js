/**
 * Handles authentication events.
 * See: http://ember-simple-auth.com/api/classes/ApplicationRouteMixin.html#method_sessionAuthenticated
 * @module instance-initializers/session-events
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export function initialize(instance) {
  // const applicationRoute = instance.container.lookup('route:application');
  const session = instance.container.lookup('service:session');
  session.on('authenticationSucceeded', () => {
    console.debug('authentication succeeded!');
  });
  session.on('invalidationSucceeded', () => {
    console.debug('session has been invalidated!');
  });
}

export default {
  initialize,
  name:  'session-events',
  after: 'ember-simple-auth'
};
