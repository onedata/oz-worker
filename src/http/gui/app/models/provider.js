import DS from 'ember-data';

export default DS.Model.extend({
  name: DS.attr('string'),
  isWorking: DS.attr('boolean', {defaultValue: false}),
  isDefault: DS.attr('boolean', {defaultValue: false}),
  spaces: DS.hasMany('space', {async: true}),

  /** North */
  latitude: 50.068918,
  /** East */
  longitude: 19.909258,

  url: 'http://example.com',

  /** Is provider selected in GUI */
  isSelected: false,
});
