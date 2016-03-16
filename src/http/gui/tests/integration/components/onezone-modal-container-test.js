import { moduleForComponent, test } from 'ember-qunit';
import hbs from 'htmlbars-inline-precompile';

moduleForComponent('onezone-modal-container', 'Integration | Component | onezone modal container', {
  integration: true
});

test('it renders', function(assert) {
  
  // Set any properties with this.set('myProperty', 'value');
  // Handle any actions with this.on('myAction', function(val) { ... });" + EOL + EOL +

  this.render(hbs`{{onezone-modal-container}}`);

  assert.equal(this.$().text().trim(), '');

  // Template block usage:" + EOL +
  this.render(hbs`
    {{#onezone-modal-container}}
      template block text
    {{/onezone-modal-container}}
  `);

  assert.equal(this.$().text().trim(), 'template block text');
});
