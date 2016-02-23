import Ember from 'ember';
import PageBase from './_page-base';

export default PageBase.extend({
  name: 'index',
  didTransition() {
    console.debug('hello');
  },
  activate() {
  console.debug('app activate');
  Ember.run.scheduleOnce('afterRender', this, function() {
    let resizeAtlasSection = function() {
      let atlasSection = $('.row-atlas');
      atlasSection.css('height', 'auto');
      let currentHeight = atlasSection.height();
      let computedHeight = $(window).height() - atlasSection.offset().top;
      if (computedHeight > currentHeight) {
        atlasSection.height(computedHeight);
      }
    };

    resizeAtlasSection();
    $(window).resize(resizeAtlasSection);

    return true;
  });
},
});
