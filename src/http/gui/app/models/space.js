import DS from 'ember-data';

/**
 * Space representation.
 * @module models/space
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  name: DS.attr('string'),
  isDefault: DS.attr('boolean', {defaultValue: false}),
  providers: DS.hasMany('provider', {async: true})
});
