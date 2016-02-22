import Ember from 'ember';
import snakeToCamel from '../utils/snake-to-camel';

export default Ember.Component.extend({
  tagName: 'ul',
  classNames: 'nav navbar-nav application-nav',
  menuItems: [],
  setupMenuItems: function() {
    let itemNames = [
      'get-started', 'documentation', 'community', 'download', 'support',
      'media', 'blog', 'account', 'login', 'logout'
    ];

    let menuItems = itemNames.map((name) => {
      return {
        linkTo: name,
        titleI18n: `application.${snakeToCamel(name)}`,
        showAuthenticated: true,
        showUnauthenticated: true,
      };
    });

    menuItems.filter((i) => i.linkTo === 'account' || i.linkTo === 'logout').forEach((i) => {
      i.showUnauthenticated = false;
    });

    menuItems.filter((i) => i.linkTo === 'login').forEach((i) => {
      i.showAuthenticated = false;
    });

    this.set('menuItems', menuItems);
  }.on('init'),

});
