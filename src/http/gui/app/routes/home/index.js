import PageBase from './_page-base';

export default PageBase.extend({
  name: 'index',

  beforeModel() {
    this.transitionTo('onezone');
  }
});
