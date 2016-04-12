import Ember from 'ember';

// TODO: make a common component with op-worker; not there are component and helpers
/**
 * Inserts a icon from oneicons font.
 * Typical usage: ``{{one-icon icon='home'}}``
 * @module components/one-icon
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  tagName: 'span',
  classNames: ['one-icon'],
  classNameBindings: ['iconClass', 'colorClass', 'additionalClasses'],

  iconClass: function() {
    return `oneicon-${this.get('icon')}`;
  }.property('icon'),

  colorClass: function() {
    let color = this.get('color');
    return color ? `color-${this.get('color')}` : '';
  }.property('color'),

  additionalClasses: function() {
    return this.get('addClass');
  }.property('addClass'),

  // defaults
  icon: 'checkbox-x',
  color: '',
  addClass: '',
});
