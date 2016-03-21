import Ember from 'ember';
// import BSAccordion from 'ember-bootstrap/components/bs-accordion';

export default Ember.Component.extend({
    store: Ember.inject.service('store'),
    classNames: ['secondary-accordion', 'spaces-accordion', 'accordion-content'],

    /** If true, the createNewSpace button is a input field */
    createNewSpaceEditing: false,

    actions: {
      startCreateNewSpace: function() {
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
