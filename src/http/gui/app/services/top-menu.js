import Ember from 'ember';

export default Ember.Service.extend({
  selectItem(itemName) {
    $(`#nav-${itemName}.active`).removeClass('active');
    $(`#nav-${itemName}`).addClass('active');
  }
});
