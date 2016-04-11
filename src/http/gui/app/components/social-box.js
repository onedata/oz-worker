import Ember from 'ember';

/**
 * Renders single login button. Can optionally has a "link" property set to go
 * to a provided link instead of invoking action.
 * @module components/social-box
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  tagName: 'div',
  classNames: ['social-box-component'],

  /** Name of social/login service (eg. 'twitter') */
  type: null,

  /** Href for link when clicked */
  link: '',

  iconName: function() {
    return `social-${this.get('type')}`;
  }.property('type'),

  hasLink: function() {
    let link = this.get('link');
    return link && link.length !== 0;
  }.property('link'),

  actions: {
    authenticate() {
      this.sendAction('action', this.get('type'));
    }
  }
});
