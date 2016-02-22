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

    let toggleButton = $('.navbar-toggle')[0];

    $('.global-fog').click(function() {
      if ($(this).hasClass('active')) {
        toggleButton.click();
      }
    });

    $('#navbar a').click(function() {
      toggleButton.click();
    });
  }
});
