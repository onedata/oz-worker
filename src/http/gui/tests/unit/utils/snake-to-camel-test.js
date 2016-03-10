import snakeToCamel from '../../../utils/snake-to-camel';
import { module, test } from 'qunit';

module('Unit | Utility | snake to camel');

test('dashed-words should be converted to camelCase format by default', function(assert) {
  let result = snakeToCamel('hello-world');
  assert.equal(result, 'helloWorld');
});

test('single word should be leaved unmodified', function(assert) {
  let result = snakeToCamel('hello');
  assert.equal(result, 'hello');
});

test('function supports underscore when custom separator provided', function(assert) {
  let result = snakeToCamel('hello_world', '_');
  assert.equal(result, 'helloWorld');
});
