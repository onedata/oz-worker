import Ember from 'ember';

let logoFor = function(name, format) {
  format = (format || 'svg');
  return `/assets/images/index/logo-${name}.${format}`;
};

export default Ember.Component.extend({
  /** jQuery element for owl-carousel */
  owl: null,

  classNames: 'owl-carousel-component',
  tagName: 'div',

  // TODO: link currently not used
  supporters: [
    {
      link: 'http://www.agh.edu.pl',
      image: logoFor('agh')
    },
    {
      link: 'https://www.poig.2007-2013.gov.pl/english',
      image: logoFor('ie'),
      boxClasses: 'background-contain'
    },
    {
      link: '#',
      image: logoFor('indigo')
    },
    {
      link: '#',
      image: logoFor('plgrid')
    },
    {
      link: '#',
      // TODO: the logo is poor quality
      image: logoFor('egi')
    },
    {
      link: '#',
      image: logoFor('cyfronet')
    }
  ],

  didInsertElement() {
    this.set('owl', this.$().find('.owl-carousel'));
    let owl = this.get('owl');

    owl.owlCarousel({
      autoPlay: 4000,
      items : 6,
      pagination: false,
    });

    // Custom Navigation Events
    this.$().find('.owl-carousel-arrows .next').click(function() {
        owl.trigger('owl.next');
    });

    this.$().find('.owl-carousel-arrows .prev').click(function() {
        owl.trigger('owl.prev');
    });

    this.$().find('.owl-carousel-arrows').height(owl.height());

    let prevButton = this.$().find('.prev');
    let nextButton = this.$().find('.next');
    [prevButton, nextButton].forEach((button) => {
      button.css('margin-top', owl.height()/2 - button.height()/2);
    });
  }
});
