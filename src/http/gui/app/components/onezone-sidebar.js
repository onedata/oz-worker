import Ember from 'ember';

/**
 * Main onezone sidebar.
 * @module
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  /** Providers list should be injected */
  providers: null,
  /** Spaces list should be injected */
  spaces: null,
  /** AuthAccounts list should be injected */
  authAccounts: null,
  /** Tokens list sholud be injected (from model) */
  tokens: null,
});
