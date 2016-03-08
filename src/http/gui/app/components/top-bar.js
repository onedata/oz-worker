import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'nav',

  classNames: ['navbar', 'navbar-onedata'],

  userName: function() {
    return this.get('session.sessionDetails.userName');
  }.property('session.sessionDetails'),

  didInsertElement() {
    let box = $('#navbar');
    let boxElement = box[0];

    box.mousewheel(function(e) {
      let isMobileMenu = $(this).hasClass('in');
      if (isMobileMenu) {
        let delta = e.originalEvent && e.deltaY;
        let scrollBegin = (this.scrollTop === 0);
        let scrollEnd = (boxElement.scrollHeight - boxElement.scrollTop === boxElement.clientHeight);
        let isScrollingUp = (delta > 0);
        if ((scrollBegin && isScrollingUp) || (scrollEnd && !isScrollingUp)) {
          e.preventDefault();
        }
      }
    });

    $('.global-fog').mousewheel(function(event) {
      event.preventDefault();
    });

    // use only one toggle button
    let toggleButton = $('.navbar-toggle')[0];

    $('.global-fog').click(function() {
      if ($(this).hasClass('active')) {
        toggleButton.click();
      }
    });

    // this should be the same code as in navbar-toggle...
    // maybe TODO: create: showMenu and hideMenu functions
    $('#navbar a').not('.dropdown-toggle').click(function() {
      // TODO: make a util which will add test element if it does not exists
      let isMobile = $('#desktop-test').is(':hidden');
      if (isMobile) {
        toggleButton.click();
      }
    });
  }
});
