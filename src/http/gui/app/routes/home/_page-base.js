import Ember from 'ember';

/**
 * Not a standard route.
 *
 * A base class for homepage routes. It is used for historical reasons, and is
 * used widely only in homepage frontend. In standard OZ front end, only
 * login page uses it.
 * @module routes/home/_page-base
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend({
  topMenuService: Ember.inject.service('top-menu'),

  activate() {
    this.get('topMenuService').selectItem(this.get('name'));
  }
});
