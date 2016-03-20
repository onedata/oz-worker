import { moduleForComponent, test } from 'ember-qunit';
import hbs from 'htmlbars-inline-precompile';

moduleForComponent('onezone-atlas', 'Integration | Component | onezone atlas', {
  integration: true
});

test('it renders', function(assert) {
  
  // Set any properties with this.set('myProperty', 'value');
  // Handle any actions with this.on('myAction', function(val) { ... });" + EOL + EOL +

  this.render(hbs`{{onezone-atlas}}`);

  assert.equal(this.$().text().trim(), '');

  // Template block usage:" + EOL +
  this.render(hbs`
    {{#onezone-atlas}}
      template block text
    {{/onezone-atlas}}
  `);

  assert.equal(this.$().text().trim(), 'template block text');
});
