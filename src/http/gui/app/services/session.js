import Ember from 'ember';
import SessionService from 'ember-simple-auth/services/session';

export default SessionService.extend({
  server: Ember.inject.service('server'),

  sessionInitResolve: null,
  sessionInitReject: null,
  sessionRestoreResolve: null,
  sessionRestoreReject: null,
  sessionValid: null,

  /** Returns a promise that will be resolved when the client has resolved
   * if it has session using WebSocket.
   * NOTE: This requires server service and WebSocket adapter.
   * If this is called, session data from websocket will resolve session
   * restoration rather than run authenticate. */
  initSession: function () {
    let session = this;
    let onOpen = () => {
      // Ask the server for session details when the WebSocket connection
      // is established
      session.resolveSession();
    };
    let onError = () => {
      // Reject session restoration if WebSocket connection
      // could not be established
      let initRejectFunction = this.get('sessionInitReject');
      if (initRejectFunction) {
        console.log("SESSION INIT REJECTED");
        initRejectFunction();
      }
      let restoreRejectFunction = this.get('sessionRestoreReject');
      if (restoreRejectFunction) {
        console.log("SESSION RESTORE REJECTED");
        restoreRejectFunction();
      }
      this.set('sessionInitResolve', null);
      this.set('sessionInitReject', null);
      this.set('sessionRestoreResolve', null);
      this.set('sessionRestoreReject', null);
    };
    this.get('server').initializeWebSocket(onOpen, onError);
    return new Ember.RSVP.Promise((resolve, reject) => {
      // This promise will be resolved when WS connection is established
      // and session details are sent via WS.
      this.set('sessionInitResolve', resolve);
      this.set('sessionInitReject', reject);
    });
  },

  /** If this is called, session data from websocket will resolve session
   * restoration rather than run authenticate. */
  tryToRestoreSession: function () {
    return new Ember.RSVP.Promise((resolve, reject) => {
      console.log('tryToRestoreSession, sessionValid = ', this.get('sessionValid'));
      if (this.get('sessionValid') === true) {
        resolve();
      } else {
        // This promise will be resolved when WS connection is established
        // and session details are sent via WS.
        this.set('sessionRestoreResolve', resolve);
        this.set('sessionRestoreReject', reject);
      }
    });
  },

  resolveSession: function () {
    console.log('session.resolveSession');
    // Request session data
    this.get('server').sessionRPC().then((data) => {
      console.log("RESOLVE SESSION REQ");
      console.log('data: ' + JSON.stringify(data));
      if (data.sessionValid === true) {
        this.get('session').set('opData', data);
        let sessionRestoreResolveFun = this.get('sessionRestoreResolve');
        if (sessionRestoreResolveFun) {
          console.log("SESSION VALID, RESTORED");
          sessionRestoreResolveFun();
        } else {
          console.log("SESSION VALID, AUTHENTICATED");
          this.get('session').authenticate('authenticator:basic');
        }
      } else {
        console.log("SESSION INVALID");
        let sessionRestoreRejectFun = this.get('sessionRestoreReject');
        if (sessionRestoreRejectFun) {
          sessionRestoreRejectFun();
        }
      }
      let resolveFunction = this.get('sessionInitResolve');
      resolveFunction();
      this.set('sessionInitResolve', null);
      this.set('sessionInitReject', null);
      this.set('sessionRestoreResolve', null);
      this.set('sessionRestoreReject', null);
    });
  }
});
