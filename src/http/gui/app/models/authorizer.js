import DS from 'ember-data';

export default DS.Model.extend({
  /**
    Provider of authentication.
    Allowed: github, plgrid, google, dropbox, facebook
  */
  type: DS.attr('string'),
  email: DS.attr('string')
});
