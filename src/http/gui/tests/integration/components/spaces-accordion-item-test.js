import { moduleForComponent, test } from 'ember-qunit';
import hbs from 'htmlbars-inline-precompile';

moduleForComponent('spaces-accordion-item', 'Integration | Component | spaces accordion item', {
  integration: true
});

test('it renders', function(assert) {
  
  // Set any properties with this.set('myProperty', 'value');
  // Handle any actions with this.on('myAction', function(val) { ... });" + EOL + EOL +

  this.render(hbs`{{spaces-accordion-item}}`);

  assert.equal(this.$().text().trim(), '');

  // Template block usage:" + EOL +
  this.render(hbs`
    {{#spaces-accordion-item}}
      template block text
    {{/spaces-accordion-item}}
  `);

  assert.equal(this.$().text().trim(), 'template block text');
});
