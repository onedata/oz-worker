import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'nav',

  classNames: ['navbar', 'navbar-onedata'],

  userName: function() {
    return this.get('session.opData.sessionDetails.userName');
  }.property('session.opData'),

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
    $('#navbar a').click(function() {
      $('.global-fog').removeClass('active');
    });
  }
});
