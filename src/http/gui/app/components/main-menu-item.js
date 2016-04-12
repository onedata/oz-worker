import Ember from 'ember';

/**
 * Item in main menu in Homepage mode
 * @module components/main-menu-item
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  tagName: 'li',
  linkTo: null,
  titleI18n: null,

  navId: function() {
    return `nav-${this.get('linkTo')}`;
  }.property('linkTo')
});
