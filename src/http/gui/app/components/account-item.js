import Ember from 'ember';

/**
  Properties:
  - label: string
  - type: string, one of: google, plgrid, dropbox, google, facebook, github
  - accepted: true/false
*/
export default Ember.Component.extend({
  classNames: ['account-item'],
  iconName: function() {
    return `social-${this.get('type')}`;
  }.property('type'),

  click() {
    this.sendAction('action', this.get('type'));
  }
});
