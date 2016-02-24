import DS from 'ember-data';

export default DS.Model.extend({
  name: DS.attr('string'),
  isWorking: DS.attr('boolean', {defaultValue: false}),
  isDefault: DS.attr('boolean', {defaultValue: false}),
  spaces: DS.hasMany('space', {async: true})
});
