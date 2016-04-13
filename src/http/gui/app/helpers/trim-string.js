import Ember from 'ember';

/**
 * Trims a JS String to length provided as a first parameter.
 * The result string will be of provided lenght _including_ "...".
 * E.g. ``{{trim-string "some long string" 5}}`` -> ``som...``
 * @module helpers/trim-string
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export function trimString(params/*, hash*/) {
  let text = params[0];
  if (text) {
    let length = params[1] || 15;

    let addEllipsis = (text.length > length);

    return addEllipsis ? (text.substring(0, length-3) + '...') : text;
  } else {
    return text;
  }
}

export default Ember.Helper.helper(trimString);
