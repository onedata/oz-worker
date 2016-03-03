import Ember from 'ember';
import bindFloater from '../utils/bind-floater';

export default Ember.Component.extend({
  onezoneServer: Ember.inject.service('onezoneServer'),
  classNames: ['account-add', 'account-item'],

  didInsertElement() {
    let popup = this.$().find('.account-add-popup');
    let updater = bindFloater(popup);
    this.$().on('mouseover', updater);
    $('.accordion-container').on('scroll', updater);
  },

  actions: {
    connectNewAccount(providerName) {
      this.get('onezoneServer').getConnectAccountEndpoint(providerName).then((url) => {
        window.location = url;
      });
    }
  }
});



// <div class="dropdown side-dropdown">
//   <a href="#" class="dropdown-toggle side-dropdown-toggle"
//     data-toggle="dropdown" role="button" aria-haspopup="true"
//     aria-expanded="false" {{action 'getNewSupportToken'}}>
//     <div class="account-icon account-add-icon">
//       {{one-icon icon='add'}}
//     </div>
//     <div class="account-label connect-new">
//       {{t 'onezone.accountAdd.connectNewAccount'}}
//     </div>
//   </a>
//   <div class="dropdown-menu dropdown-menu-right floater account-add-popup">
//     {{!-- TODO: style and copy button --}}
//     {{!-- TODO: loading icon --}}
//     <strong>ROTFL</strong>
//     {{account-item type='google' label='connect by google'}}
//     {{account-item type='facebook' label='connect by google'}}
//     {{account-item type='github' label='connect by google'}}
//     {{account-item type='google' label='connect by google'}}
//     {{account-item type='google' label='connect by google'}}
//     {{account-item type='google' label='connect by google'}}
//   </div>
// </div>
