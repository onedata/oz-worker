import DS from 'ember-data';

export default DS.Model.extend({
  name: DS.attr('string'),
  isDefault: DS.attr('boolean', {defaultValue: false}),
  providers: DS.hasMany('provider', {async: true})
});
