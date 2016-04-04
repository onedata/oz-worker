import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['provider-place'],
  classNameBindings: ['isWorking'],

  isWorking: function() {
    return this.get('provider.isWorking') ? 'working' : '';
  }.property('provider.isWorking'),

  /** A provider model that will be represented on map */
  provider: null,

  ICON_AR_X: 0.052344031*0.5,
  ICON_AR_Y: 0.10648918*0.5,

  // TODO: dynamic width/height, currently set as in styles
  width: function() {
    return this.get('atlas.width')*this.ICON_AR_X;
  }.property('atlas.width'),

  height: function() {
    return this.get('atlas.height')*this.ICON_AR_Y;
  }.property('atlas.height'),

  // TODO: optimize
  sizeChanged: function() {
    let circle = this.$().find('.circle');
    circle.css({
      fontSize: this.get('width')*0.75 + 'px',
      lineHeight: this.get('height')*0.90 + 'px',
      width: this.get('width') + 'px',
      height: this.get('height') + 'px',
    });

    // circle.css('fontSize', );
    // circle.width(this.get('width'));
    // circle.height(this.get('height'));
  }.observes('width', 'height'),

  posY: function() {
    return this.get('atlas.centerY') -

      (this.get('provider.latitude')/90)*(this.get('atlas.height')/2) -
      (this.get('height')/2);
  }.property('provider.latitude', 'atlas.centerY'),

  posX: function() {
    return this.get('atlas.centerX') +
      (this.get('provider.longitude')/180)*(this.get('atlas.width')/2) -
      (this.get('width')/2);
  }.property('provider.longitude', 'atlas.centerX'),

  positionChanged: function() {
    if (this.get('posX') && this.get('posY')) {
      this.$().css('left', `${this.get('posX')}px`);
      this.$().css('top', `${this.get('posY')}px`);
    }
  }.observes('posX', 'posY'),

  isActive: function() {
    return this.get('provider.isSelected');
  }.property('provider.isSelected'),

  didInsertElement() {
    this.positionChanged();
    this.sizeChanged();
  },

  actions: {
    toggleActive() {
      this.sendAction('selectProvider', this.get('provider'));
    }
  }
});
