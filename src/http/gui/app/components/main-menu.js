import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'ul',
  classNames: 'nav navbar-nav application-nav',
  menuItems: [
    {linkTo: 'get-started', titleI18n: 'application.getStarted'},
    {linkTo: 'documentation', titleI18n: 'application.documentation'},
    {linkTo: 'community', titleI18n: 'application.community'},
    {linkTo: 'download', titleI18n: 'application.download'},
    {linkTo: 'support', titleI18n: 'application.support'},
    {linkTo: 'media', titleI18n: 'application.media'},
    {linkTo: 'blog', titleI18n: 'application.blog'},
    {linkTo: 'login', titleI18n: 'application.login'},
  ],
});
