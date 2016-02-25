/**
 * Creates some hard-coded records with random relations in store.
 *
 * Firstly, removes records (all of given models) then creates new.
 * Other models are leaved untouched.
 *
 * @module initializers/fixtures
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
*/

import Ember from 'ember';

function clearStore(store, successCallback) {
  store.findAll('provider').then((providers) => {
    let destroyPromises = providers.toArray().map((s) => s.destroyRecord());

    Ember.RSVP.all(destroyPromises).then(() => {
      return (successCallback && successCallback());
    });
  });
}

export function initialize(container /*, application */) {
  //let store = container.lookup('service:store');
  //
  //clearStore(store, () => {
  //  let providers = ['Amazon', 'Dropbox', 'Google', 'HBP Cyfronet'];
  //  providers = providers.map((name, i) => store.createRecord('provider', {id: i+1, name: name}));
  //  providers[0].isWorking = false;
  //  providers[3].isDefault = true;
  //  providers.forEach(s => s.save());
  //
  //  let spaces = ['Cyfronet data', 'Documentation', 'My data'];
  //  spaces = spaces.map((name, i) => store.createRecord('space', {id: i+1, name: name}));
  //  spaces[2].get('providers').pushObjects(providers);
  //  spaces.forEach(s => s.save());
  //});
}

export default {
  name: 'fixtures',
  after: 'store',
  initialize
};
