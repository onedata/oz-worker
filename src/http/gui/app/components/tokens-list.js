import Ember from 'ember';

/**
 * List of cliettokens. A container
 * @module components/tokens-list
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  store: Ember.inject.service('store'),
  classNames: ['secondary-accordion', 'tokens-accordion', 'accordion-content'],

  actions: {
    /** Creates new clienttoken record on server - its id will be a token to display */
    createNewToken: function() {
      let token = this.get('store').createRecord('clienttoken', {});
      token.save();
    },
  }
});
