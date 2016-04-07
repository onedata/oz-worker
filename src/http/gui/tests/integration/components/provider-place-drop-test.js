import { moduleForComponent, test } from 'ember-qunit';
import hbs from 'htmlbars-inline-precompile';

moduleForComponent('provider-place-drop', 'Integration | Component | provider place drop', {
  integration: true
});

test('it renders', function(assert) {
  
  // Set any properties with this.set('myProperty', 'value');
  // Handle any actions with this.on('myAction', function(val) { ... });" + EOL + EOL +

  this.render(hbs`{{provider-place-drop}}`);

  assert.equal(this.$().text().trim(), '');

  // Template block usage:" + EOL +
  this.render(hbs`
    {{#provider-place-drop}}
      template block text
    {{/provider-place-drop}}
  `);

  assert.equal(this.$().text().trim(), 'template block text');
});
