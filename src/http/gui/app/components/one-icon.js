import Ember from 'ember';

export default Ember.Component.extend({
  classNameBindings: ['icon-name', 'color'],
  iconName: 'oneicon-checkbox-x',
  color: function() {
    let colorClass = this.get('colorClass');
    return colorClass ? `color-${this.get('colorClass')}` : '';
  }.bind('colorClass')
});
