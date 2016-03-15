export function initialize(container, application) {
  application.inject('component', 'session', 'service:session');
  application.inject('route', 'session', 'service:session');
}

export default {
  name: 'logger',
  after: 'ember-simple-auth',
  initialize: initialize,
};
