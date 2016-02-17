import Ember from 'ember';

export default Ember.Component.extend({
  /** Name of social/login service (eg. 'twitter') */
  type: null,

  /** Href for link when clicked */
  link: '',

  hasLink: function() {
    let link = this.get('link');
    return link && link.length !== 0;
  }.property('link'),

  tagName: 'div',
  classNames: ['social-box-component']
});
