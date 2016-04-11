import DS from 'ember-data';

/**
 * A oneprovider representation, available for user.
 * @module
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  name: DS.attr('string'),
  isWorking: DS.attr('boolean', {defaultValue: false}),
  isDefault: DS.attr('boolean', {defaultValue: false}),
  spaces: DS.hasMany('space', {async: true}),

  /** North */
  latitude: DS.attr('number'),
  /** East */
  longitude: DS.attr('number'),

  /** Is provider selected in GUI */
  isSelected: false
});
