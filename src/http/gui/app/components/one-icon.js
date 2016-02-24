import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'span',
  classNameBindings: ['iconClass', 'colorClass'],

  iconClass: function() {
    return `oneicon-${this.get('icon')}`;
  }.property('icon'),

  colorClass: function() {
    let color = this.get('color');
    return color ? `color-${this.get('color')}` : '';
  }.property('color'),

  // defaults
  icon: 'checkbox-x',
  color: ''
});
