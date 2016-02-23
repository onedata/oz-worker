import Ember from 'ember';
import config from './config/environment';

const Router = Ember.Router.extend({
  location: config.locationType
});

Router.map(function() {
  this.route('home', function() {
    [
      'get-started', 'documentation', 'community',
      'download', 'support', 'media', 'blog',
      'login', 'account', 'logout'
    ].forEach((homepagePage) => {
      this.route(homepagePage);
    });
  });

  this.route('onezone', function() {});
});

export default Router;
