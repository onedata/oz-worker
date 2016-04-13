import Ember from 'ember';
import PageBase from './_page-base';
import UnauthenticatedRouteMixin from 'ember-simple-auth/mixins/unauthenticated-route-mixin';

let LoginRoute = PageBase.extend(UnauthenticatedRouteMixin);

/**
 * A login page, only true "home subpage" in standard OZ GUI. Show login widgets.
 * @module routes/home/login
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default LoginRoute.extend({
  session: Ember.inject.service('session'),
  onezoneServer: Ember.inject.service('onezoneServer'),
  name: 'login',
  zoneName: null,

  beforeModel() {
    if (this.get('session.isAuthenticated')) {
      this.transitionTo('onezone');
    }
  },

  model() {
    return new Ember.RSVP.Promise((resolve/*, reject*/) => {
      this.get('onezoneServer').getZoneName().then(
        (zoneName) => {
          resolve({
            zoneName: zoneName
          });
        },
        () => {
          console.error('Failed to get zone name');
          resolve({
            zoneName: null,
          });
        }
      );
    });
  }

  //actions: {
  //  authenticate(providerName) {
  //    this.get('onezoneServer').getLoginEndpoint(providerName).then((url) => {
  //      window.location = url;
  //    });
  //  }
  //}
});
