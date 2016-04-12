import Ember from 'ember';

/**
 * A list of spaces. Container for spaces-accordion-items.
 * @module components/spaces-accordion
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
    store: Ember.inject.service('store'),
    classNames: ['secondary-accordion', 'spaces-accordion', 'accordion-content'],

    /** If true, the createNewSpace button is a input field */
    createNewSpaceEditing: false,

    spaces: null,
    spacesSorting: ['isDefault:desc', 'name'],
    spacesSorted: Ember.computed.sort('spaces', 'spacesSorting'),

    actions: {
      startCreateNewSpace: function() {
        this.set('createNewSpaceName', null);
        if (!this.get('createNewSpaceEditing')) {
          this.set('createNewSpaceEditing', true);
        }
      },

      // TODO: this should be invoked when pressing Esc when in editing mode
      // TODO: maybe create a global object, in which any cancel action can be registered
      // TODO: eg. service('cancel').register(fun);
      cancelCreateNewSpace: function() {
        if (this.get('createNewSpaceEditing')) {
          this.set('createNewSpaceEditing', false);
        }
      },

      endCreateNewSpace: function(spaceName) {
        try {
          let store = this.get('store');
          let space = store.createRecord('space', {
            name: spaceName,
          });
          // TODO: handle errors
          space.save().then(() => {
            // TODO: some animation on new space entry?
            // logger.debug(`Space ${spaceName} created successfully`);
          });
        } finally {
          this.set('createNewSpaceEditing', false);
        }
      }
    }
});
