import Ember from 'ember';
import config from './config/environment';

const Router = Ember.Router.extend({
  location: config.locationType
});

Router.map(function() {
  this.route('get-started');
  this.route('documentation');
  this.route('community');
  this.route('download');
  this.route('support');
  this.route('media');
  this.route('blog');
  this.route('login');
  this.route('account');
  this.route('logout');
});

export default Router;
