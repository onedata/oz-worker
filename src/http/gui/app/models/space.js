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

  /** isDefault is old form of "is home a space" - only one user space can be default */
  isDefault: DS.attr('boolean', {defaultValue: false}),

  /** List of models of providers that support this space */
  providers: DS.hasMany('provider', {async: true})
});
