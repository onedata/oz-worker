# Release notes for project oz_worker


CHANGELOG
---------

### 3.0.0-rc7

* VFS-2567 Rework space_record API in space data backend
* VFS-2607 Fix a bug in group_logic can_view_public_data
* VFS-2607 Fix a bug in group_logic has effective user
* VFS-2607 Add public group type for groups that users do not have view data privs
* VFS-2607 Allow viewing group details for users that have view privs in any space that contains the group
* VFS-2607 Allow access to public data of groups and spaces for users without view privileges
* VFS-2567 Check view permissions during space find in GUI backend
* VFS-2607 Introduce env variable deciding about automatic first space creation
* VFS-2567 Add verification of view perms in provider GUI backend
* VFS-2607 Add better error reporting in REST user PATCH
* VFS-2607 Add OIDC integration with EGI
* VFS-2607 Fix onepanel users not being added to global groups
* VFS-2548 Disable global groups by default
* VFS-2548 Fix bugs in space name mapping mechanism, add CT tests verifying automatic space membership by global grups
* VFS-2548 Add overwrite argument to set_space_name_mapping function
* VFS-2548 Add mechanism for automatic groups in OZ
* VFS-2469 Add modifying handles via proxy.
* VFS-2469 Add registering and unregistering of handles via proxy.
* VFS-2469 Update cluster_worker and cluster_manager, add loading of gen_server2 to relx.
* VFS-2405 Treat share as different record than space, create corresponding modules
* VFS-2405 Update share redirection path to provider
* VFS-2407 Use separate record for shares rather then space
* VFS-2469 Reorganize rest_modules_test_SUITE and add empty test mocks for handles management.
* VFS-2397 Implement oai_handler


### 3.0.0-rc6

* VFS-2582 Using GUI fix for blank notifications
* VFS-2390 Upgrade rebar to version 3


### 3.0.0-rc5

* VFS-2491 Add RPC call to join a space
* VFS-2468 Add log level opt to location service script
* VFS-2468 Add nodejs to packages requirements
* VFS-2500 described location service config options
* VFS-2309 no more bootstrap on testmaster
* VFS-2309 adjusted location service to use node 4
* VFS-2176 all 'A' DNS entries used as bootstrap nodes
* VFS-2309 boostrap location service client on testmaster
* VFS-2309 rest auth skeleton
* VFS-2309 public keys retrieved from directly publishers
* VFS-2309 refactoring: encoded key pased & utitlities separated
* VFS-2309 identity verification in oz
* VFS-2309 location service added


### 3.0.0-rc4

* VFS-2156 Change onepanel config env variables types
* Disable openid providers in auth.config by default, leaving basicAuth only


### 3.0.0-RC3

* VFS-2156 Remove GUI files
* VFS-2436 Allow modifying user alias via REST
* VFS-2436 Disallow adding inexistent users or groups to spaces via REST admin endpoint
* VFS-2154 Fix a mixup between privileges expressed in atoms and binaries in rest privileges tests
* VFS-2154 Update privileges rest handler to accept patch rather than put
* VFS-2154 Use all possible combinations of privs rather than random subset
* VFS-2154 Major refactor of rest privileges test suite and some improvements to the tested code
* VFS-2154 Further refinement of OZ API privileges tests, update oz-gui-default reference
* VFS-2358 HOTFIX: Fix a badmatch in provider data backend
* VFS-2358 Update comments concerning module APIs in space and provider data backends
* Releasing new version 3.0.0-RC2
* VFS-2154 Rework tests for OZ API, fix some bugs that were revealed by tests
* VFS-2273 Handle macaroon verification errors
* VFS-2358 Implement unsupport space functionality in GUI
* VFS-2357 Add checks if taking away privileges works in OZ API REST
* VFS-2269 Enable Symmetric Multiprocessing
* VFS-2357 Add admin endpoints to add/remove users and groups from spaces, fix a couple of bugs
* VFS-2359 Turn off HSTS by default, allow configuration via app.config, improve docs integration
* VFS-2359 Add handler for serving static docs files located on another server
* Releasing new version 3.0.0-RC1
* VFS-2250 Use wrappers for macaroon serialization


### 3.0.0-RC2

* VFS-2357 Add checks if taking away privileges works in OZ API REST
* VFS-2269 Enable Symmetric Multiprocessing
* VFS-2357 Add integration tests for OZ API REST functionalities
* VFS-2357 Add admin endpoints to add/remove users and groups from spaces, fix a couple of bugs
* VFS-2351 onedata_auth is persistent
* VFS-2359 Turn off HSTS by default, allow configuration via app.config, improve docs integration
* VFS-2359 Add handler for serving static docs files located on another server


### 3.0.0-RC1

* VFS-2316 Update etls.
* VFS-2250 Use wrappers for macaroon serialization
* VFS-2121 Tests refactoring


### 3.0.0-beta8

* minor changes and improvements


### 3.0.0-beta7

* VFS-2225 Update GUI docker image
* bugfix - cleaning changed users list


### 3.0.0-beta6

* Update erlang tls
* VFS-2133 Rework google and indigo auth logic
* VFS-2111 Integrate user management with onepanel
* VFS-2111 Enable changing password only if user has basic auth enabled
* VFS-2111 Add several funcitons to admin API - list spaces, list providers, list providers of space
* VFS-2111 Add rest handler for OZ API privileges manipulation
* Fix space name mapping after space removal
* VFS-2111 Implement basic login backend
* VFS-2111 allow basic auth in all user requests
* VFS-2111 Allow using client token for authorization in REST, provider certs are no longer obligatory when not required, add REST API to get client_token
* VFS-2111 Change values returned from rpc_backends to JSON objects rather than strings
* VFS-2087 supported spaces are not public
* VFS-2087 push provider updates to all


### 3.0.0-beta5

* VFS-2068 adjust to new webscoket adapter API
* VFS-1987 concurrent refreshes and updates test
* VFS-1987 subscriptions updates with nested groups of users groups
* VFS-1987 set & get for nested group privileges
* VFS-1987 get effective user in rest
* VFS-1987 nested groups in global config (json)
* VFS-1987 nested groups rest privileges
* VFS-1987 nested groups in subscriptions
* VFS-1987 effective groups in user document
* VFS-1987 general graph traversal
* VFS-1987 effective users in logic
* VFS-1987 model changes


### 3.0.0-beta4

* Minor updates.


### 3.0.0-beta3

* VFS-1860 explicit default space added
* VFS-1860 trimmed (with public data only) users are always pushed
* VFS-1825 rework gui starting in zone up
* VFS-1768: Do not allow provider drop above onezone modals
* VFS-1768: Do not allow scroll bars on atlas
* VFS-1607 Add space canonical name to get data response in user context.
* VFS-1596 Update getting onedata user.
* VFS-1607 Save space name mapping in user document.
* VFS-1596 More detailed get_data for user.
* VFS-1768: Spinners in login boxes
* VFS-1770 fix disappearing client tokens


### 3.0.0-beta1

* VFS-1770 make sure provider is inoperable before displaying it
* VFS-1768: Token copy button in modals; dynamic page titles
* VFS-1521 remove providerId restriction on new tokens
* VFS-1757 Change application ports availability checking procedure.
* VFS-1792 moving privilages to ctool
* VFS-1796 location as optional create args
* VFS-1629 covered cache malfunctions
* VFS-1629 covered fetching old changes from db
* VFS-1629 limiting db fetch life
* VFS-1629 connected provider to the OZ (over websocket)
* VFS-1629 subscribtions over websocket
* VFS-1629 sending info needed by op
* VFS-1629 user subscriptions
* VFS-1629 client subscriptions
* VFS-1629 extracted outbox
* VFS-1629 buffering outbox
* VFS-1629 extraction of subscription handling
* VFS-1629 introduction of modules: cache, subscribers, translator
* VFS-1629 provider can obtain only spaces on his own
* VFS-1629 subscriptions via rest


### 3.0.0-alpha3

* VFS-1638 enable simple auth mixins and adjust redirection pages after login
* VFS-1672: New callbacks for login create
* VFS-1672: Conditionally show/hide modals on onezone
* VFS-1638 add support for custom GUI
* VFS-1672: User dropdown on the right
* VFS-1672: New homepage account dropdown style
* VFS-1672: New rendering of social icons in onezone
* VFS-1638 add real user credentials to backend
* VFS-1672: Onezone panels
* VFS-1638 differentiate between chosen provider and default provider
* VFS-1672: Updated oneicons 1.3
* VFS-1665 Pull in Macaroons.
* VFS-1544 distributed gr is packaged via onezone repo
* VFS-1672: Fixed main menu blinking on click
* VFS-1544 uuids easier to use with http
* VFS-1638 switch to hash based location service, use server backend for spaces and providers
* VFS-1638 enable login for all oauth providers
* VFS-1638 allow logging in with openid providers
* VFS-1638 allow pages without .html extension to server index.html
* VFS-1638 add new gui_livereload modes
* VFS-1544 updating CW and adapting to refactored dns
* VFS-1672: New routes and draft of onezone layout
* VFS-1636: Red menu highlight on top (z dimension) of menu line
* VFS-1638 add polling and watching options for gui livereload


### 3.0.0-alpha2

* VFS-1665 Pull in Macaroons.


### 3.0.0-alpha

* VFS-1622 Add openssl to package requirements.
* VFS-1520-delete annotations from all ct_tests
* VFS-1528 Remove deprecated use of erlang:now/0
* VFS-1428 Add endpoint that allows for getting token issuer.
* VFS-1378 adjust to the new ctool API
* VFS-1223 add rest port to macaroon's location
* VFS-1223 Handle empty macaroon-discharges header.
* VFS-1223 Parse macaroons from HTTP headers.
* VFS-1223 Implement first revision of macaroon-based auth.
* VFS-1123 Use Macaroons for tokens.
* itegrate ssl2
* VFS-914, DNS server in GR supports aliases
* VFS-914, add DNS to GR



### 2.1.0

* Better behaviour when GUI window is small
* Provider instruction updated



### 2.0.0

* Support for spaces
* Support for logging with Google, Facebook, Dropbox, Github and PL-Grid
* User account management enabled
* Support for tokens



________

Generated by sr-release. 
