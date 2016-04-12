export default {
  common: {
    fetchingToken: 'Fetching token...'
  },
  application: {
    title: 'Onezone',

    getStarted: 'get started',
    documentation: 'documentation',
    community: 'community',
    download: 'download',
    support: 'support',
    media: 'media',
    blog: 'blog',
    account: 'account',
    onezone: 'providers',
    login: 'login',
    logout: 'logout'
  },
  index: {
    atlas: {
      subtitle: 'Global data access solution for science',
      text: 'Have the best of both worlds! Perform heavy computations on huge datasets. ' +
      'Access your data in a dropbox-like fashion regardless of its location. ' +
      'Publish and share your results with public or closed communities.',
      getStarted: 'Get started'
    },
    whatIs: {
      title: 'What is onedata?',
      text: 'High-performance data management solution that offers unified data access ' +
      'across globally distributed environments and multiple types of underlying ' +
      'storage, allowing users to share, collaborate and perform computations on ' +
      'the stored data easily.'
    },
    why: {
      title: 'Why onedata?',
      virtualized: {
        title: 'Virtualized data access',
        text: 'Single virtual storage on top of multiple physical file systems ' +
        'distributed over any type of storage.'
      },
      performance: {
        title: 'High performance',
        text: 'Direct, block level data access and on-the-fly data prefetching ' +
        'allow for effictient data intensive computations.'
      },
      infrastructure: {
        title: 'Infrastructure flexibility',
        text: 'Tailored for Cloud, Grid, HPC and Desktop environments; compatible ' +
        'with enterprise-grade Linux distributions.'
      },
      opendata: {
        title: 'Support for open data',
        text: 'Easy sharing of large datasets with dedicated users of publishing ' +
        'them as open data.'
      }
    },
    join: {
      title: 'Join onedata',
      user: {
        title: 'Become a user',
        text: 'Enjoy anytime, anyplace access to your data with easy collaboration ' +
        'and support for high-performance computations.'
      },
      provider: {
        title: 'Become a provider',
        text: 'Support global scientific research with yout storage resources.'
      }
    },
    forResearchers: {
      title: 'For researchers by researchers!',
      text: 'Developed in a scientific community with hands-on experience in Grids, ' +
      'data-intensive applications and storage systems with quality of ' +
      'service guarantees. Our system strives to fill the gap between ' +
      'science oriented, well-served data solutions and consumer-grade' +
      'cloud storage services.'
    },
    whoSupports: {
      title: 'Who supports us?'
    },
    knowMore: {
      title: 'Want to know more? Talk to us',
      privacy: 'Privacy',
      tos: 'ToS',
      copyright: '&copy; 2016 Onedata.org'
    }
  },
  login: {
    title: 'Login',

    boxTitle: 'login',
    boxSubtitle: 'Login with your social account',
    unknownZoneName: 'unknown'
  },
  onezone: {
    title: 'Manage account',

    messages: {
      noneProviders: {
        title: 'All your providers are offline',
        text: 'Currently, all the providers that support your spaces ' +
        'are offline. This means that you cannot get access to your files ' +
        'until they come back online. If you get additional support for your' +
        'space you will be able to use it but you will not see the files that' +
        'are stored in offline providers. You can also create new spaces ' +
        'and ask for support.'
      },
      getSupport: {
        title: 'Get support',
        text: 'Currently, none of your spaces is supported by any provider. ' +
        'You need support from at least one provider to store data in ' +
        'a space. To get support, go to "Manage Data Spaces" menu, get a ' +
        'support token for chosen space and pass it to a provider of your choice.'
      },
      firstLogin: {
        title: 'First login',
        p1: 'You have successfully logged in and an account for you has been created.',
        p2: 'You are now on account management page. Here, you can connect other ' +
        'accounts to your profile, modify your user data, manage your spaces' +
        'and providers.',
        p3: 'We have created a default space for you. You need to find a ' +
        'provider that will support it before you can store your files.'
      }
    },
    sidebar: {
      connectHead: 'connect your accounts',
      manageSpacesHead: 'manage data spaces',
      filesHead: 'go to your files',
      tokensHead: 'manage client tokens',
      aliasHead: 'set user alias'
    },
    topBar: {
      manageProviders: 'manage account',
      enableHints: 'enable hints'
    },
    accountsList: {},
    accountAdd: {
      connectNewAccount: 'Connect new account'
    },
    spacesAccordion: {
      createNewSpace: 'Create new space',
      createNewSpaceText: 'You can also become a provider yourself and support ' +
      'your own space.',
      readMore: 'Read more'
    },
    spacesAccordionItem: {
      getSupport: 'Get support'
    },
    providersAccordion: {
      chooseProvider: 'Choose Provider',
      noProviders: 'No providers, get support first.'
    },
    providersAccordionItem: {},
    tokensAccordion: {
      createNewToken: 'Create new client token'
    },
    tokensAccordionItem: {},
    providerPlaceDrop: {
      operable: 'Operable',
      inoperable: 'Inoperable',
      goToFiles: 'Go to your files'
    },
    aliasPanel: {
      noAlias: 'You have no alias'
    }
  }
};
