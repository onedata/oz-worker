import Ember from 'ember';

export default Ember.Component.extend({
  /** Providers list should be injected */
  providers: null,
  /** Spaces list should be injected */
  spaces: null,
  /** AuthAccounts list should be injected */
  authAccounts: null,
  /** Tokens list sholud be injected (from model) */
  tokens: null,

  // didInsertElement() {
  //   let box = $('.accordion-container');
  //   let boxElement = box[0];
  //
  //   box.mousewheel(function(e) {
  //     let delta = e.deltaY;
  //     console.log(box.css('top'));
  //     let scrollBegin = (parseInt(box.css('top')) === 80);
  //     let scrollEnd = (parseInt(box.css('top') === (box.height()+80)));
  //     let isScrollingUp = (delta > 0);
  //     if (!(scrollBegin && isScrollingUp) && !(scrollEnd && !isScrollingUp)) {
  //       box.css('top', `${parseInt(box.css('top'))+delta}px`);
  //     }
  //   });
  // }
});
