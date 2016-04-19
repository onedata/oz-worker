import Ember from 'ember';

/**
 * A service to manipulate top-menu of Homepage globally.
 * @module services/top-menu
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Service.extend({
  selectItem(itemName) {
    $(`#nav-${itemName}.active`).removeClass('active');
    $(`#nav-${itemName}`).addClass('active');
  }
});
