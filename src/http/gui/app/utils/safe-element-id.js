// TODO: move this to common gui files
/**
 * Convert string to safe element id for HTML.
 * @module utils/safe-element-id
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/**
 * Returns element id string safe for HTML.
 *
 * @param {string} elementId Old id text to make safe
 * @returns {string} safe id
 */
export default function safeElementId(elementId) {
  return elementId.replace(/[^\w-]/g, '');
}
