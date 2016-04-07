import Ember from 'ember';

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
