import Ember from 'ember';
import PageBase from './_page-base';

/** Match first "slide" (onedata world map) to screen height */
let resizeAtlasSection = function() {
  let atlasSection = $('.row-atlas');
  atlasSection.css('height', 'auto');
  let currentHeight = atlasSection.height();
  let computedHeight = $(window).height() - atlasSection.offset().top;
  if (computedHeight > currentHeight) {
    atlasSection.height(computedHeight);
  }
};

export default PageBase.extend({
  name: 'index',
  activate() {
    Ember.run.scheduleOnce('afterRender', this, function() {
      resizeAtlasSection();
      $(window).on('resize', resizeAtlasSection);
      return true;
    });
  },

  deactivate() {
    $(window).off('resize', resizeAtlasSection);
    return true;
  },
});
