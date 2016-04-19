import DS from 'ember-data';

/**
 * A login account of user, using remote services, eg. OAuth2.
 * @module modals/authorizer
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  /**
    Provider of authentication.
    Allowed: github, plgrid, google, dropbox, facebook
  */
  type: DS.attr('string'),
  email: DS.attr('string')
});
