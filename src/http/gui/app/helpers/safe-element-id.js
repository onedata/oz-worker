import Ember from 'ember';
import safeElementIdUtil from '../utils/safe-element-id';

/**
 * Using safeElementId util, convert string to safe element id.
 * @module utils/safe-element-id
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export function safeElementId(params/*, hash*/) {
  return safeElementIdUtil(params[0]);
}

export default Ember.Helper.helper(safeElementId);
