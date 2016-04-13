import PageBase from './_page-base';

/**
 * Just try to redirect to onezone - user will be redirected to login page if not logged.
 * @module routes/home/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default PageBase.extend({
  name: 'index',

  beforeModel() {
    this.transitionTo('onezone');
  }
});
