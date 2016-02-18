import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'nav',
  classNames: ['navbar', 'navbar-onedata'],

  userName: function() {
    return this.get('session.opData.sessionDetails.userName');
  }.property('session.opData'),
});
