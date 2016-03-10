import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'li',
  linkTo: null,
  titleI18n: null,

  navId: function() {
    return `nav-${this.get('linkTo')}`;
  }.property('linkTo')
});
