import Ember from 'ember';

let accountIconPath = function(type) {
  return `/assets/images/onezone/account-${type}.svg`;
};

/**
  Properties:
  - label: string
  - type: string, one of: google, plgrid, dropbox, google, facebook, github
  - accepted: true/false
*/
export default Ember.Component.extend({
  classNames: ['account-item'],
  typeIcon: function() {
    return accountIconPath(this.get('type'));
  }.property('type')
});
