# Release notes for project oz-worker

## CHANGELOG

### 21.02.7

-   **VFS-12080** Web GUI: Added support for creating custom login view
    pages.

### 21.02.6

-   **VFS-12197** Web GUI: Improved share list views.
-   **VFS-12195** The name of the handle service where an Open Data
    handle has been registered is now publicly visible and always
    displayed on the Share view.
-   **VFS-12110** Added a circuit breaker mechanism that disables all
    Onezone services when the database is close to running out of disk
    space.

### 21.02.5

-   **VFS-11912** Web GUI: Added visual editor for Open Data metadata in
    Europeana Data Model format.
-   **VFS-11761** Web GUI: Improved user experience in publishing share
    as Open Data.
-   **VFS-11760** Web GUI: Fixed shares sidebar crash when user does not
    have view privilege for some space.
-   **VFS-11744** Web GUI: Refactored UX of changing provider settings
    in cluster.
-   **VFS-11710** Web GUI: added possibility to embed Onedata Web
    application into an iframe hosted on site with domain other than
    Onezone.
-   **VFS-11707** Web GUI: Detecting web browser extensions potentially
    interfering with Onedata GUI.
-   **VFS-11595** Web GUI: Fixed missing application elements when Qwant
    VIPrivacy add-on is installed in user's browser.
-   **VFS-11594** Web GUI: Fixed QoS query builder "Add" button
    out-of-the-screen placement.
-   **VFS-11582** Web GUI: Fixed showing tokens list when one of invite
    targets gets deleted.
-   **VFS-11365** Improvements to Open Data publishing and the OAI-PMH
    protocol implementation. Added support for Europeana Data Model
    metadata type. Improved the metadata editor in UI (both the visual
    and XML forms). Improved the conformity to OAI-PMH protocol spec and
    added support for optional functionalities. Added an HTTPS endpoint
    (next to previously supported HTTP).
-   **VFS-11317** Web GUI: Major refactor of members view to show direct
    and effective privileges with memberships on a single view.
-   **VFS-11080** Web GUI: Fixed share disappearing from the list after
    delete failure.
-   **VFS-10265** Changes to user spaces (changing space name,
    joining/leaving a space) are now reflected in Oneclient in real
    time. Added support for spaces with the same name in Oneclient - in
    case of an ambiguity a space name is extended with its ID.
-   **VFS-9157** Web GUI: Added support for advanced file info in file
    selector panel.

### 21.02.4

-   **VFS-10999** Added the public "infer access token scope" endpoint,
    which analyses the provided access token to infer the scope of data
    access the token can be used for.
-   **VFS-10649** Web GUI: added complex charts dashboard editor to the
    automation GUI.

### 21.02.3

-   **VFS-11231** Refactored oneclient connection pool to improve
    stability.
-   **VFS-11100** Web GUI: Added support for global URLs to files for
    authenticated users.
-   **VFS-10962** Web GUI: Improved WebSocket auto-reconnect after
    connection lost (e.g. on system suspend).
-   **VFS-10895** Added detailed specification of needed file properties
    in "file" automation type, including narrowing file properties set
    passed to lambda functions during automation workflow exectution.
-   **VFS-10782** Added the possibility to state the reason when
    rejecting a membership request posted via the Space Marketplace.
-   **VFS-10664** Web GUI: Space Marketplace Service Pack 1, including:
    reject confirmation view, new information tiles on space overview,
    space configuration unsaved changes guard, UX fixes.


### 21.02.2

-   **VFS-10889** Web GUI: Added information about unsupported features
    in older Oneproviders version supporting space.
-   **VFS-10825** Web GUI: Fixed unnecessary scrollbars in Safari.
-   **VFS-10819** Web GUI: Fixed atm. inventory invite tokens creation
    in tokens page.
-   **VFS-10612** Add a journal that records the starts and shutdowns of
    services and procedures that help to ensure the graceful stopping of
    services.
-   **VFS-10411** Web GUI: Added support for incomplete infinite scroll
    listing chunks from backend. It increases stability of file browser
    GUI.

### 21.02.1

-   **VFS-10531** Web GUI: Added support for enum-like data types in
    automation (list of allowed values for string and number types).
-   **VFS-10432** Web GUI: Fixed cookie notification appearing after
    consenting and starting new web browser session.
-   **VFS-10414** Web GUI: Fixed displaying wrong names of spaces in
    shares sidebar.
-   **VFS-10389** Web GUI: Added showing data per provider in directory
    statistics.
-   **VFS-10357** Web GUI: Added support for "manage archives" privilege
    and "creator" information in archives.
-   **VFS-10312** Web GUI: Changed name conflict and disambiguation
    separator from `#` to `@`.
-   **VFS-10235** Changed default value of `accountingEnabled` parameter
    to `true` when requesting space support.
-   **VFS-10129** Web GUI: Added global and per-lane chart dashboards in
    automation workflows.
-   **VFS-10128** Web GUI: Added possibility to map a lambda result to
    many stores in an automation task definition.
-   **VFS-10126** Web GUI: Added Space Marketplace - space
    configuration, spaces browser and membership requests resolver
    views.
-   **VFS-10125** Added mailing capabilities to Onezone by means of an
    integrated SMTP client.
-   **VFS-10122** Introduced the Space Marketplace. Any space can be
    publicly advertised in the Marketplace, allowing Onedata users to
    request access to it. The access requesting process is based on an
    email exchange with the appointed space maintainer.
-   **VFS-10118** Web GUI: Introduced GUI for automation lambda
    parameters.
-   **VFS-10117** Added lambda config specification and the possibility
    to provide its values on the task level. Changed data types in
    automation; added boolean, reworked integer into number with
    constraints.
-   **VFS-10065** Web GUI: Fixed API samples tab in file details modal
    for archived file.
-   **VFS-10046** Web GUI: Added user details popovers with user
    information in various views.
-   **VFS-10037** Web GUI: Added "rate" and "timeDerivative" time series
    chart functions.
-   **VFS-9999** Web GUI: Added space details popovers with space
    information in various views.
-   **VFS-9637** Web GUI: Improved and unified look of audit log
    browsers.
-   **VFS-9622** Upgraded the base image for release dockers from Ubuntu
    18.04 to Ubuntu 20.04.
-   **VFS-9614** Web GUI: Added archivisation audit log view.
-   **VFS-9531** Web GUI: Improved UX of the automation stores browser.
-   **VFS-9339** Web GUI: Added a new file info tab with REST API
    samples.
-   **VFS-9207** Web GUI: Enforced loading fonts from server to avoid
    incorrect local fonts in browser.
-   **VFS-9162** Web GUI: Added API samples modal for space.
-   **VFS-9129** Web GUI: Upgraded EmberJS to v3.8.
-   **VFS-9036** Web GUI: Added showing time series charts in executed
    automation workflows.
-   **VFS-8948** Introduced the concept of time series dashboard specs -
    structured recipes for transforming and displaying measurements in
    charts, which can be organized into sections. Each time series
    collection has a linked dashboard spec that will be used for
    rendering the dashboard in GUI.
-   **VFS-8903** Web GUI: Upgraded EmberJS to v3.4.
-   **VFS-8716** Web GUI: Added complex data types editor to the
    automation views.
-   **VFS-8681** Added a new data type to automation machinery - the
    array type, with recursive specification of the data type of its
    elements.
-   **VFS-8638** All lambdas now work in batch mode - the lambda creator
    must handle the input with batch arguments and produce an output
    with batch results. The batch size used during workflow execution is
    controlled by the parameters `maxBatchSize` in lane schema
    definitions and `preferredBatchSize` in lambda definitions.
-   **VFS-8348** Web GUI: added links to transferred files on transfers
    view and information about their membership in archive and dataset.
-   **VFS-8288** It is now possible to specify requested resources and
    resource limits on the lambda and task level for OpenFaaS functions.
-   **VFS-8263** Added blocking modal when user tries to exit editor
    with unsaved workflow.
-   **VFS-8247** Added new option to harverster's indices that allow for
    harvesting details about archives (archiveId, archiveDescription and
    archiveCreationTime).
-   **VFS-8172** Add `/health` endpoints to REST APIs of all services.
-   **VFS-8073** Upgrade folly, wangle and proxygen libraries to version
    2021.01.04.00.
-   **VFS-8043** Added support for the time series data type in
    automation models.
-   **VFS-7975** Added possibility to cancel running automation
    workflow.
-   **VFS-7947** Added possibility to run workflows directly from file
    browser.
-   **VFS-7900** Added the possibility to unlink unused lambdas from an
    automation inventory. Upon unlinking from its last inventory, the
    lambda is automatically removed.
-   **VFS-7898** Web GUI: added self-shortening links to files with
    support for files inside archives.
-   **VFS-7880** Introduce the concept of automation; tools for defining
    and executing automated workflows, made up of lambdas that are
    submitted to a local OpenFaaS platform. These functionalities
    currently have experimental status.
-   **VFS-7873** Web GUI: Added possibility to dump, upload and
    duplicate automation lambdas.
-   **VFS-7846** Added action "Upload BagIt" to file browser, which is
    available when OpenFaaS and special "BagIt uploader" workflow are
    available.
-   **VFS-7829** Add the possibility to export workflow schemas to JSON
    and import them based on the JSON, linking or creating missing
    lambdas in one shot.
-   **VFS-7817** GUI improvements in automation GUI: added navigation
    via URL to specific execution, creating stores during task and lane
    creation, showing inventory name for each execution entry.
-   **VFS-7747** Upgrade the codebase to Erlang OTP 24.
-   **VFS-7738** Fixed issues with navigation between datasets, archives
    and archive files browsers.
-   **VFS-7728** Introduced versioning of lambdas and workflow schemas.
    Users may create subsequent revisions of the above models and modify
    their statuses (draft, stable, deprecated) to simplify management
    and retain backward compatibility of definitions that are already in
    use.
-   **VFS-7724** Web GUI: redesigned datasets and archives browser to
    browse datasets tree and archives in single splitted view.
-   **VFS-7715** Web GUI: Added store references check to automation
    workflow editor.
-   **VFS-7705** Added more file actions to archive file browser GUI:
    share, metadata, permissions read, data distribution and quality of
    service.
-   **VFS-7663** Changed background image of sign-in page in Onezone and
    Onepanel.
-   **VFS-7648** Added Web GUI views for browsing and creating datasets
    and archives.
-   **VFS-7633** UX improvements in web GUI concerning navigation
    between files, datasets and archives using hyperlinks.
-   **VFS-7629** Web GUI: added new datasets panel with archives browser
    in file browser.
-   **VFS-7575** Add the possibility to incorporate an XRootD server
    within the Onedata environment for exposing Open Data collections
    for public access using the XRootD protocol.
-   **VFS-7515** Added new harvesting options - fileType and
    datasetInfo.
-   **VFS-7429** Implemented the concept of datasets. Datasets allow the
    space users to organize their data into collections with desired
    granularity. A file or directory marked as a dataset offers
    additional features, such as optional data and metadata protection
    or the ability to create persistent snapshots of the physical
    dataset contents. In case of a directory, a dataset covers all its
    subdirectories and files. Datasets can be nested, allowing users to
    compose arbitrary hierarchical structures. Added corresponding views
    for managing datasets and write protection flags in file browser Web
    GUI and a REST API, accessible under `/datasets`,
    `/datasets/{DatasetId}` and `/data/{FileId}/dataset/summary` paths.
-   **VFS-7329** Added automation GUI - inventories, lambdas and
    workflows views in Onezone and workflows execution overview in
    Oneprovider.
-   **VFS-7304** Add preliminary REST API for dataset archivization - to
    be extended in near future.
-   **VFS-7228** Improve the design and UX of the token consumer page in
    Onezone GUI .

### 20.02.19

-   **VFS-9926** Web GUI: Opening Oneprovider view in Onezone GUI when
    entering Oneprovider domain.
-   **VFS-9730** Improve handling of api caveats related to API
    operations concerning other components than Onezone - in some cases,
    tokens with such caveats could be treated as invalid.
-   **VFS-10012** Web GUI: Fixed hanging file upload bug when multiple
    uploads have been scheduled.

### 20.02.18

-   **VFS-9771** Web GUI: Added handling of the restricted registration
    policy in the "Add cluster" page.

### 20.02.17

-   **VFS-9475** Web GUI: Better handling of the restricted registration
    policy in the tokens generator web tool.
-   **VFS-9474** Improved behaviour of the Oneprovider deployment page
    in Spaces GUI when the registration policy is set to restricted.

### 20.02.16

-   **VFS-9182** Improved EGI group mapping for special COU groups
    \"admins\" and \"members\"; their names are now concatenated with
    parent group names for better presentation. Additionally, \"admins\"
    groups now get admin privileges in their parent groups instead of
    default member privileges.

### 20.02.15

-   **VFS-8630** Added support for displaying optional Terms of Use
    document.

### 20.02.14

-   **VFS-8482** Added dedicated page for privacy policy content.
-   **VFS-8326** Web GUI: added service name and domain information on
    control panel login screen.

### 20.02.13

### 20.02.12

-   **VFS-8196** Add support for sets in OAI PMH. Every handle service
    is considered a separate set, and all handles created using a handle
    service belong to its corresponding set.

### 20.02.11

-   **VFS-7741** Web GUI: showing proper error information page when
    there is no on-line supporting provider for share.

### 20.02.10

### 20.02.9

### 20.02.8

### 20.02.7

-   **VFS-7413** Fixed displaying users with the same full name on
    members views.
-   **VFS-7321** Added REST API endpoint for blocking and unblocking
    user accounts. Blocked user is denied access to all Onedata services
    unless their account is unblocked again.
-   **VFS-7294** Added publicly available REST endpoints for fetching
    information and data of shared files/directories. The data-related
    endpoints are offered by Onezone, which redirects to a suitable
    Oneprovider so that a guest user does not need any knowledge of the
    environment to access the data. Improved the Web GUI's shares view
    to present the public endpoints in an easy-to-use manner.
-   **VFS-7165** Add a workaround for Erlang's SSL implementation that
    would not reload server certificate chain when it is changed (e.g.
    after Let's Encrypt certificate regeneration).
-   **VFS-6638** Fixed handling duplicate clusters states on clusters
    list in GUI.
-   **VFS-6566** Improved UX and fixed minor issues in share views.
    Fixed inability to open share hosted by Oneprovider 19.02.x using
    Onezone 20.02.x.

### 20.02.6

-   **VFS-6802** Added visual QoS expression editor with live matching
    storages evaluation.

### 20.02.5

-   **VFS-6999** Improve error reporting in entrypoints of
    oneprovider/onezone dockers, always dump application logs to stdout
    in case of failures during batch deployment.
-   **VFS-6745** Added new view with token templates in tokens creator
    GUI.

### 20.02.4

-   **VFS-6958** Added new endpoint that checks correctness of a QoS
    expression and returns the list of storages that match the
    expression.
-   **VFS-6841** Introduce unified time management in all Onedata
    components - all clusters now regularly synchronize their clocks
    with the Onezone service, the process is managed by Onepanel's
    master node.


### 20.02.3

### 20.02.2

-   **VFS-6853** Matching session cookie is now required to verify a GUI
    access tokens (they are used behind the scenes by the Onedata web
    applications), which increases security.
-   **VFS-6852** Fixed Oneprovider and Onezone services not showing in
    tokens creator's service caveat list when user had no access to
    service cluster.
-   **VFS-6845** Prevent application from stopping until all documents
    are correctly persisted in order to improve resistance to temporary
    database errors.
-   **VFS-6732** New JSON and RDF metadata editor based on Ace Editor.
-   **VFS-6563** All entities avaliable in Onezone REST (users, group,
    spaces, providers etc) now include the creator and creation time in
    the returned details.
-   **VFS-6456** Show more details about lack of privileges when trying
    to perform various actions in GUI.


### 20.02.1

-   **VFS-6645** Optimize changes querrying.
-   **VFS-6628** Extended harvesting configuration - it is now possible
    to select harvesting options, like metadata types to harvest or
    additional file details (fileName, spaceId), upon index creation.
    New metadata types are now harvestable - xattrs and rdf. Default
    values of HarvestingBackendType and HarvestingBackendEndpoint can
    now be set by Onezone admin - if set, these values can be omitted
    when creating a new harvester. New option (retry\_on\_rejection)
    allowing for all payloads rejected by the harvesting backend to be
    automatically analysed for offending data (e.g. fields that do not
    match the schema), pruned and submitted again.
-   **VFS-6607** Fix node restart with HA disabled.
-   **VFS-6568** Introduced concept of readonly storage. If enabled,
    Oneprovider will block any operation that writes, modifies or
    deletes data on the storage. Such storage can only be used to import
    data into the space. Mandatory to ensure proper behaviour if the
    backend storage is actually configured as readonly.
-   **VFS-6518** Endpoint is now optional when adding a new harvester -
    if not provided, the default one from Onezone configuration will be
    used. Generic GUI is now loaded by default upon harvester creation.
-   **VFS-6474** Added initial support for XRootD storage, including
    direct access to XRootD storages and importing of legacy data sets
    stored on XRootD or EOS servers.
-   **VFS-6462** Every file in a space is now harvested - including
    files without metadata. Harvested metadata now has a new key -
    \`\_\_onedata\` - which contains file name, space id and xattrs.
-   **VFS-6457** Added new publicly visible field to shares -
    description (supports the markdown format).
-   **VFS-6453** New Open Data and share description views with visual
    Dublin Core editor and Markdown editor.
-   **VFS-6421** New generic GUI plugin for harvesters.
-   **VFS-6378** Onepanel GUI and REST API now explicitly block
    supporting a space with more than one imported storage (globally) -
    such operation was possible in the past but was never supported by
    the internal storage import logic and led to incoherent view on
    space data.
-   **VFS-6358** Optimization of files upload through GUI.
-   **VFS-6346** GUI improvements: added Oneprovider GUI notifications,
    better file selection, additional error handling, better file
    manager refresh UX, fixed overflow of context menu in file browser,
    fixes in responsive layout.
-   **VFS-6344** GUI: showing information if QoS requirement is
    impossible to be fulfilled.
-   **VFS-6343** Added delete account feature in GUI.
-   **VFS-6288** Basic HA functionality (experimental) - protect
    Oneprovider from single node failure.
-   **VFS-6287** Integrate traverse pools with HA sub-system.
-   **VFS-6261** Integrate high-level services with HA sub-system.
-   **VFS-6184** Added the space owner concept. Space owner works like
    \"root\" within the space - such user is allowed to perform all
    file/API operations, regardless of the assigned privileges and file
    permissions / ACLs. Ownership can be assigned to any number of
    users, and it is forbidden to leave a space without an owner -
    ownership must be transferred first.
-   **VFS-6167** Allow nodes adding and deleting in-fly basing on HA
    sub-system.
-   **VFS-5648** Extended QoS expression to allow comparators (\<, \>,
    \<=, \>=) and numeric values. Changed \"-\" operator to \"\\\".
    Space characters (\" \"), dashes (\"-\") and underscores (\"\_\")
    are now allowed in QoS parameters. Added more details to invalid QoS
    expression errors.
-   **VFS-4760** Added implicit API caveats that limit access tokens
    used by Onedata GUIs behind the scenes for authentication and
    authorization. Different services in the system are presented with
    user\'s access token with power limited to bare minimum required for
    the service to handle user requests. For example, Oneproviders do
    not have access to APIs that could alter or delete user data and
    memberships.

### 20.02.0-beta4

### 20.02.0-beta3

-   VFS-5901 Application config can now be customized with arbitrary
    number of config files added to /etc/oz\_worker/config.d/ directory.

-   VFS-5730 Introduced differentiation of named and temporary tokens
    (applicable to both access and invite tokens). Named tokens are
    assigned a name and are retrievable from the Onezone GUI or REST
    API. They can be revoked, unrevoked or deleted at will. Temporary
    tokens are valid for a limited time, cannot be retrieved or deleted
    and share a common secret (can be revoked all at once for given
    subject). Temporary tokens do not have any persistence and can be
    created in unlimited amounts.

-   VFS-5524, VFS-5735 Invite and access tokens can now contain
    customizable caveats that confine the context in which the token is
    valid. This concept is taken from Google\'s macaroons, which are
    in fact used for underlying implementation. Example caveats include:
    audience, IP address, ASN, geographical region, system\'s
    interface.

-   VFS-5727 REST API for tokens is now available, allowing to create
    named and temporary tokens with caveats and including a wide range
    of token management endpoints.

-   VFS-5874 Data access caveats are now supported in tokens. They are
    treated as special caveats that reduce the available API to a bare
    minimum required solely for data access. Tokens with such caveats
    can only be used in Oneclient or Oneprovider\'s REST & CDMI. Data
    access caveats include: readonly caveat (allowing readonly access),
    data path caveat (restricting the logical file paths that can be
    accessed) and objectid caveat (restricting the accessible file IDs).

-   VFS-5733 The concept of \\\"storage\\\", representing the
    Oneprovider\'s storage system on which the space data is stored,
    has been moved to Onezone. From now on, Oneproviders can share
    information about storage parameters defined by Oneprovider admins
    and use this knowledge in QoS algorithms that manage data replicas.

-   VFS-5899 GUI update \* New tokens gui

-   VFS-6250 Changed after\_init callback to on\_cluster\_ready

-   VFS-5983 Update consistent\_hashing usage

-   VFS-5983 Support datastore HA

-   VFS-6231 Added upgrade essential workers in node manager

-   VFS-5944 Add GUI translator for token examine endpoint, include
    invite target name info in case of invite tokens

-   VFS-6185 Update cluster\_worker to support documents\' timestamps

-   VFS-5944 Add GUI translator for zone time

-   VFS-5944 Fix temp token\'s max ttl check

-   VFS-5944 Add GUI translator for user temporary tokens

-   VFS-5944 Add max\_temporary\_token\_ttl to GUI GS handshake
    attributes

-   VFS-6150 Make sure entity graph is reconciled after changing user
    privileges in invite tokens test SUITE

-   VFS-6174 Update ctool and cluster-worker refs, add missing REST
    testcase for support parameters update operation

-   VFS-6150 Fix a regression in OIDC code parsing headers in IDP
    responses

-   VFS-6043 Add space support related concepts: parameters, dbsync
    state and provider\'s support state *Add corresponding API* Ensure
    backward compatibility and upgradeability

-   VFS-6129 Increased timeout in elasticsearch plugin

### 19.02.5

### 19.02.4

-   **VFS-6402** Disallowed creating more than one public handle for a
    single share.
-   **VFS-6401** All authentication errors are now wrapped in
    UNAUTHORIZED error and map to 401 HTTP code to avoid ambiguity when
    reporting token related errors - tokens can be used for
    authentication as well as input data for some operations (e.g.
    invite tokens).
-   **VFS-6390** Because of asynchronous processing, it was possible
    that GraphSync session cleanup intertwined with deleted record
    cleanup (that removes corresponding subscriptions from sessions,
    possibly including the session being cleaned up) and caused an error
    that interrupted change propagation. Now, if the session is no
    longer existent, subscription removal errors are ignored and the
    propagation completes.
-   **VFS-6369** Fix datastore internal call, batch management during
    links listing and infinite loop during storage directories creation.

### 19.02.3

-   Releasing new version 19.02.3

### 19.02.2

-   VFS-6210 Move plugins to the data dir (/var/lib/oz\_worker/) rather
    than etc dir as they should not be persisted between upgrades
-   VFS-6129 Increased timeout in elasticsearch plugin
-   VFS-6019 Add better status tracking of user and provider
    connections, display it in db\_browser
-   VFS-6060 Fixed public access to gui plugin config in harvester
-   VFS-6035 Add VM option that forbids terminating the node with Ctrl +
    C
-   VFS-6011 Update DNS SOA serial on each config generation

### 19.02.1

-   VFS-6019 Add db\_browser.sh script for viewing Onezone database
    contents (admin tool)
-   VFS-5936 Improve entitlement mapping *Merge entitlements with
    different roles (privileges) to the highest of them* Store previous
    privileges to discover and coalesce later changes in user roles
-   VFS-5940 Rename GUI package verification envs to more intuitive
-   VFS-5205 Hide share CREATE and DELETE operations from Onezone REST
    API (as they are reserved for Oneprovider logic), return rootFileId
    as ObjectID in share details

### 19.02.0-rc2

-   VFS-1891 GUI update Added privacy policy and cookie consent
    notification
-   VFS-5708 Implement gui\_message management through zone\_logic
-   VFS-5708 Migrate login\_notification configured in app.config to
    gui\_message model
-   VFS-5699 Implement cluster upgrade procedure

### 19.02.0-rc1

-   VFS-5689 Increase default GUI token TTL
-   VFS-5678 change indices privileges to views privileges
-   VFS-5524 Add MaxMind\'s GeoLite2 database for geolocation queries
-   VFS-5635 Make all records added to DNS server lowercase
-   VFS-4893 Created endpoint to retrieve full list of privileges
-   VFS-5498 Enable GUI package verification by default
-   VFS-5657 Enabled Ubuntu distribution package tag
-   VFS-5544 Fix create operation always returning the revision of
    original resource (rather than the newly created)
-   VFS-5498 Do not check oz-worker gui-sha256 (OZ GUI needn\'t be
    verified)
-   VFS-5498 Disable GUI verification until acceptance tests are aware
    of it
-   VFS-5498 Enable GUI package verification by default, fix errors in
    user cleanup procedure
-   VFS-5544 Add awareness of record revision to GraphSync
-   VFS-5551 Allow nobody auth override
-   VFS-5508 Add browserDebugLogs field to GUI context
-   VFS-4698 Implemented onepanel rest proxy
-   VFS-4547 update ctool and od\_space rec to account for privs changes
-   VFS-5398 Restore the order in which auth is checked in REST
-   VFS-5398 Implement the concept of audience in authorization *Rework
    GUI tokens* Reorganize code related to tokens of all kinds *Refactor
    code related to auth* Prepare ground for comprehensive tokens
    refactor

### 18.02.3

-   Releasing new version 18.02.3

### 18.02.2

-   VFS-5194 Rewritten model upgrade tests to use datastore

### 18.02.1

-   VFS-5189 Add support for owner role in EGI group parser
-   VFS-5154 Clean no\_dot\_erlang scripts
-   VFS-5154 Fixed folly dependency in RPM spec
-   VFS-5161 Check the deprecated configuration endpoint in tests
-   VFS-5161 Rename subdomainDelegationEnabled to
    subdomainDelegationSupported
-   VFS-5161 Duplicate /configuration endpoint in a swagger-descried
    path
-   VFS-5133 Make GUI OZ session longer by default
-   VFS-5133 Add template.auth.config to onezone package
-   VFS-5106 Change the order how REST authorization is checked
-   VFS-4962 Implement openid mock server and use it in integration
    tests
-   VFS-4962 Add an extra auth\_debug log to entitlement\_mapping when
    admin group is added, implement custom\_enetitlement\_parser for
    plgrid
-   VFS-4970 Update cluster\_worker to fix cache invalidation and expire
    documents

### 18.02.0-rc13

-   VFS-4614 Adjust logs during auth.config upgrade for clearer error
    reporting
-   VFS-4623 Generate SOA admin from zone domain
-   VFS-4623 Add defaults to dns config envs
-   VFS-4623 Flatten dns config section

### 18.02.0-rc12

-   Updating GUI, including: VFS-4702-auth-icons-config \* VFS-4702
    Support for customizable authorization providers icons, colors and
    names
-   VFS-4614 Code polishing
-   VFS-4614 Universal auth.config for OIDC/SAML Identity Providers
-   VFS-4952 Update description of provider update

### 18.02.0-rc11

-   VFS-4837 Fix error in removal of nonexistent TXT record
-   VFS-4769 Optimize datastore calls
-   VFS-4571 Remove cluster\_worker config from app.config (use
    defaults)
-   VFS-4571 Modify cluster worker config for better couchbase
    performance
-   VFS-4571 Slightly refactor entity\_graph code, lower the number of
    processes waiting on entity graph lock
-   VFS-3858 Properly calculate effective relations of dirty entity
    after a relation is deleted
-   VFS-3858 Use entity\_graph relation logic in provider Graph Sync
    translator
-   VFS-3858 Use self keyword with entity type rather that direct atom
    to express direct intermediary, so as not to break record structure
-   VFS-4696 Allowed for duplicate provider domains
-   VFS-3858 Rework entity graph for better performance *Do not wait for
    full graph reconciliation after entities create / update* Calculate
    approximate effective reltion/permissions when entity is dirty
    rather then return cached value *Slightly refactor Graph Sync and
    Entity Logic, introduce different returned values for create
    operations* Adjust tests to new Entity Graph behaviour
-   VFS-4055 Update bp\_tree
-   VFS-4633 Tokens are not consumed upon failed operation

### 18.02.0-rc10

-   VFS-4695 Fix arguments order in dns soa record creation
-   VFS-4638 Added group name normalization
-   VFS-4029 Implement responding to Let\'s Encrypt http challenge
-   Updating GUI, including: \* VFS-4668-add-admin-message-to-login-page
-   VFS-4666 Add customization of brand subtitle and login notification
    (GUI) to config
-   VFS-4664 Fix a crash report when OZ discards a provider connection
-   VFS-4590 Update cluster-worker ref to include pings in Graph Sync
    connection
-   VFS-4637 Update esaml ref to include saml rollover mechanism. adjust
    the code accordingly and add an endpoint for retrieving saml
    certificate
-   VFS-4582 Hotfix - get group name through protected scope (rather
    than private) in group data backend

### 18.02.0-rc9

-   VFS-4615 Improvide provider\_logic\_plugin readability
-   VFS-4615 Allow updating provider domain to itself
-   VFS-4615 Add port number to db\_nodes env variable
-   VFS-4514 Fix tp internal calls
-   VFS-4582 Adjust EGI entitlements parsing procedure to new specs,
    ignore groups that do not conform to the specs rather then decline
    login
-   Updating GUI, including: VFS-4455-provider-space-list-fixes,
    VFS-4572-change-subtrees-to-submodules-in *VFS-4455 Fixed wrong
    position of provider spinner and handling space creation errors*
    VFS-4572 Development: using git submodules
-   VFS-4532 Update node\_package vars for all platforms
-   VFS-4532 Add autogenerated.config file to start params

### 18.02.0-rc8

-   Releasing new version 18.02.0-rc8

### 18.02.0-rc7

-   VFS-4531 Do not include dev auth.config in test release

### 18.02.0-rc6

-   Releasing new version 18.02.0-rc6

### 18.02.0-rc5

-   VFS-3953 Hotfix for broken logout page
-   VFS-3953 Integrate new GUI static backend, refactor and tidy around
    the whole project

### 18.02.0-rc4

-   VFS-4072 Provider registration disallowed when domain is occupied
-   VFS-3902 Fixed issue when deregistering provider with graph sync
-   VFS-4005 Changed validators to accept names 2 characters long

### 18.02.0-rc3

-   VFS-4005 Added name validators
-   VFS-4431 Cleanup dependencies, update meck to stabilize user\_logic
    tests
-   Fix groups not being merged when using external IdP token for
    authorization
-   VFS-4292 Removed appmock
-   4292 Changed appmock to mocked http\_client

### 18.02.0-rc2

-   VFS-4446 Enabled git archive submodules
-   VFS-4446 Updated jiffy ref
-   VFS-4446 Updated dockers.config
-   VFS-4446 Updated jiffy ref
-   VFS-4295 Changed subtrees to submodules
-   Update cluster\_worker to enable links listing with neg offset
-   VFS-4408 Removed web\_client references

### 18.02.0-rc1

-   VFS-2021 Added dockers.config

### 18.02.0-beta6

-   Releasing new version 18.02.0-beta6

### 18.02.0-beta5

-   VFS-3130 Allowed empty body in rest requests
-   VFS-3703 Switched from mochiweb json parsing to jiffy
-   VFS-4272 Check forward compatibility during OP connections to OZ
-   VFS-4267 Updated package dependencies for cberl
-   VFS-4296 Fixed meck entry
-   VFS-4267 Adjust code to erl 20, update deps

### 18.02.0-beta4

-   Updated cberl ref

### 18.02.0-beta3

-   VFS-4171 Fixed oz-worker CentOS deps
-   VFS-4171 Add folly and libcouchbase libevent plugin deps
-   Updating GUI, including: VFS-4027 \* VFS-4027 Added support for
    peta-, exa-, zetta- and yottabytes
-   VFS-3715 add case checking that group cannot join itself
-   VFS-3744 Do not treat TXT records as reserved subdomain
-   VFS-4096 add tests for provider registration token
-   VFS-3744 Update tests for static dns records
-   VFS-3744 Insert static entries from app config into dns
-   VFS-4096 add optional enforcement of tokens when registering
    providers
-   VFS-4213 Move saml and auth configs to /etc from /var/lib
-   invalidate basic auth cache for given user on his deletion
-   VFS-4054 Remove nested datastore update from domain config update
-   VFS-4054 Make default external ip undefined
-   VFS-4054 Rely on cluster worker to store external IP

### 18.02.0-beta2

-   VFS-4172 Do not set subdomain on update when delegation is disabled
-   VFS-4172 add configuration endpoint
-   VFS-4148 Allow for multiple entries with the same handler module in
    auth.config
-   VFS-4095 use map instead of proplists
-   VFS-4095 add cache for user info fetched from onepanel
-   disable http2
-   VFS-4130 Update ctool, adjust to new time\_utils API
-   VFS-4087 Rework entity graph to avoid nested record updates
-   VFS-4119 Remove static docs proxy
-   VFS-3704 update cowboy to version 2.2.2

### 18.02.0-beta1

-   VFS-3978 Do not distribute test CA with oz-worker
-   VFS-3751 Authorize providers using macaroons rather than
    certificates
-   VFS-3751 Remove OZ CA
-   VFS-3579 Generate OZ CA cert on startup if not present, add endpoint
    for publishing public CA to providers
-   VFS-3279 Implement new synchronization channel between OP and OZ
    (Graph Sync)
-   VFS-3730 Separate trusted CAs from certificate chain
-   VFS-3765 Add admin email to oneprovider data
-   VFS-3526 Reimplement DNS server to support OZ subdomains
-   Refactor datastore models to integrate them with new datastore
-   Change links storing model to use dedicated links tree for each
    provider

### 17.06.2

-   Releasing new version 17.06.2

### 17.06.1

-   Releasing new version 17.06.1

### 17.06.0-rc9

-   VFS-4004 Update ctool to include safe ciphers in TLS
-   VFS-3951 add build\_version env var for oz
-   VFS-3972 Fix attach-direct consoles in releases not being run with
    xterm terminal
-   VFS-3951 add rest endpoint for asking about oz version
-   VFS-3904 Optimize CA loop
-   VFS-3803 Add endpoint for providers returning zone time, include
    provider latitude and logitude in subscriptions

### 17.06.0-rc8

-   Releasing new version 17.06.0-rc8

### 17.06.0-rc7

-   VFS-3826 Add richer configuration options for Keycloak group mapping
-   Updating GUI, including: VFS-3710 - VFS-3710 Using binary prefixes
    for size units (IEC format: MiB, GiB, TiB, etc.)
-   Updating GUI, including: VFS-3669 - VFS-3669 Added a refresh token
    button on each tab of space support modal
-   VFS-3783 Move state tokens from single-node ETS to datastore
-   VFS-3772 Accept all VOs in EGI OIDC group mapping

### 17.06.0-rc6

-   Releasing new version 17.06.0-rc6

### 17.06.0-rc5

-   Releasing new version 17.06.0-rc5

### 17.06.0-rc4

-   Releasing new version 17.06.0-rc4

### 17.06.0-rc3

-   VFS-3455 Updating GUI ref
-   VFS-3594 Add missing validation rules for group token creation
    operations
-   VFS-3594 Fix a validation error during group token creation via POST
    and an error during user password change
-   VFS-3567 Store missing documents in datastore cache
-   VFS-3449 add endpoint for mapping groups
-   VFS-3556 Update esaml reference to support AES-CBC-256 encryption in
    SAML
-   VFS-3512 Update oz-gui-default reference
-   VFS-3473 Add support for HTTP-POST binding in SAML

### 17.06.0-rc2

-   Releasing new version 17.06.0-rc2

### 17.06.0-rc1

-   VFS-3458 Make sure user\'s connected accounts are popagated
    through subscriptions, use md5 rather than base64 to encode user and
    group ids coming from IdPs
-   VFS-3457 User base64 url rather than base64 in user id encoding
-   VFS-3448 Use single \'onedata\' bucket
-   VFS-3457 Fix a bug in groups encoding from SAML assertions, do
    base64 of user ids from IdPs
-   VFS-3429 Update esaml reference to point to repo in onedata\'s
    github
-   Reconfigure couchbase pools
-   VFS-3376 Fix exemplary saml.config

### 17.06.0-beta6

-   VFS-3376 Fix a bug making space aliases in subscriptions to not
    include effective spaces of users
-   VFS-3376 Add support for group mapping via OIDC and SAML
-   VFS-3415 Fix a routing bug causing public share links malfuntion
-   VFS-3224 Make sure that all unicode characters are properly decoded
    from SAML assertions
-   VFS-3224 Add a fix for Chrome/Safari getting stalled during SAML
    redirects
-   VFS-3224 Implement SAML login, add better error handling in login
    process

### 17.06.0-beta4

-   VFS-3386 Create new users upon IdP login with id based on IdP name
    and user id in that IdP
-   VFS-3362 Update web-client

### 17.06.0-beta3

-   VFS-3350 Make sure effective privileges are recomputed after
    creating new entities; remove deprecated privilege names
-   Releasing new version 17.06.0-beta2

### 17.06.0-beta2

-   VFS-3342 Make sure user aliases in subscriptions are precomputed
    every time a space name changes, decrease changes intervals
-   VFS-3345 Updating GUI ref (development) - changed height of textarea
    in getting support modal - truncating long provider names in space
    details
-   VFS-3286 Cluster\_worker update (update node monitoring logging)

### 3.0.0-rc16

-   VFS-3217 Rename auth\_rhea module to auth\_keycloak module
-   VFS-3217 Add support for RHEA KeyCloak OpenID Connect

### 3.0.0-rc15

-   VFS-3251 Updating GUI to 3.0.0-rc15
-   VFS-3245 Schedule effective graph refresh after entity deletion
-   VFS-3181 Add an RPC call to retrieve service version info
-   VFS-3181 Using GUI VFS-3172 with service version display
-   Add service version info to sessionDetails in GUI
-   VFS-3213 Update cberl reference
-   VFS-3213 Add libcouchbase package dependency
-   VFS-3146 Update models specyfications
-   VFS-3146 Update datastore models to use new datastore API
-   VFS-3132 Remove invalid paths from rest routes
-   VFS-3088 Integrate with new datastore

### 3.0.0-rc14

-   HOTFIX fix a bug in REST routing

### 3.0.0-rc13

-   VFS-3118 Change default env value for custom gui root
-   VFS-3097 Wait for effective graph synchronization after new space is
    created via GUI
-   VFS-3097 Allow using external access token to authorize REST
    operations

### 3.0.0-rc12

-   VFS-3006 Remove annotations.
-   VFS-2719 Do not remove relations of entity being deleted, as this
    caused unnecessary db operations
-   VFS-2719 Trigger user subscriptions upon space rename
-   VFS-2719 Add authorization case for providers accessing shares
-   VFS-2719 Use a union of direct and effective relations in
    subscriptions for faster propagation
-   VFS-2719 Do not re-check provider connectivity if subscriptions
    channel is down
-   VFS-2719 Set default entity names to empty string
-   VFS-2719 Remove entity from dirty queue if it no longer exists
-   VFS-2496 Push new provider record after unsupport space
-   VFS-2496 Push new space record after unsupport space
-   VFS-2719 Migrate all calls to user\_logic to new api
-   VFS-2719 Implement REST translators for all modules
-   VFS-2496 User real user id rather than 0 in gui backend
-   VFS-2882 Add group data backend
-   VFS-2719 Completed user REST routes
-   VFS-2882 Push providers upon group join that adds a space to user
-   VFS-2882 Add support for joining groups via gui
-   VFS-2898 Supervise ozpca process
-   VFS-2719 Add differentiation between unauthorized and forbidden in
    entity logic
-   VFS-2719 Use OZ hostname from app.config everywhere (rahter than
    from dns.config)
-   VFS-2719 Account oz privileges in effective graph
-   VFS-2719 Add support for collecting eff relationship intermediaries
-   VFs-2719 Create first placeholder for eff\_graph logic
-   VFS-2496 Allow to set null default provider or space
-   VFS-2931 Reduce number of kept rotated log files
-   VFS-2883 Add space support sizes information per provider in space
    data backend
-   VFS-2883 Add space size to space record served by space data backend
-   VFS-2883 Add provider hostname to provider record served by proivder
    data backend

### 3.0.0-rc11

-   VFS-2765 Add some error resistance when obtaining unexpected data
    from openid providers
-   VFS-2765 Update gui reference and adjust code to the new API
-   VFS-2765 Update cluster\_worker reference
-   VFS-2765 Display first login info to users logging in via
    credentials
-   VFS-2733 Add REST routes to GUI listener
-   VFS-2733 Standarize app listeners

### 3.0.0-rc10

-   VFS-2703 Update mocking
-   VFS-2703 Update datastore config
-   VFS-2720 Fix bug that did not allow providers to get provider data
-   VFS-2734 Add support for list\_groups\_of\_provider OZ privilege
-   VFS-2734 Add tests for get users of provider privilege
-   VFS-2734 Add list spaces of provider privileges
-   VFS-2720 Add better test coverage for list\_providers\_of\_space OZ
    privielges
-   VFS-2720 Add better test coverage for list\_spaces and
    list\_providers OZ privielges
-   VFS-2720 Update od\_user and og\_group record structures
-   VFS-2720 Do not generate all combinations of privileges in
    privileges tests as it takes too long
-   VFS-2720 Fix default provider redirection not working properly
-   VFS-2662 Fix a bug in change password, update gui ref and adjust the
    code to new API
-   VFS-2469 Use iso time format for serialization of timestamps, move
    serialization logic to ctool.
-   VFS-2667 Improve json encoder for DB operations
-   VFS-2593 - update ctool and cluster-worker, use STRESS\_TEST\_BASE
    macro in stress test
-   VFS-2659 Disallow creation of spaces with empty names
-   VFS-2659 Make effective children in groups a list of Ids rather than
    pairs with privileges
-   VFS-2659 Fix public user record translator in subscriptions
-   VFS-2659 Adjust subscriptions tests to refactored models
-   VFS-2659 Rename some of the key records in db
-   VFS-2640 - move all error handling to oai\_errors module
-   VFS-2659 Rename some records and record fields for consistency, add
    a lot of effective relations

### 3.0.0-rc9

-   Releasing new version 3.0.0-rc9

### 3.0.0-rc8

-   VFS-2625 Removing share\'s reference to handle is now not
    obligatory
-   VFS-2625 Fix handles not being properly retrieved via REST
-   VFS-2625 Use unencoded shalsh character in public handle field oh
    handle
-   VFS-2625 Add handle services and handles to user and group
    subscriptions
-   VFS-2625 Set default value of service properties in handle services
    to empty list
-   VFS-2625 Add tests for handle\_services subscriptions
-   VFS-2625 Fix handle subscription tests
-   VFS-2625 Add tests for handles and handle\_services subscriptions
-   VFS-2625 Add handle\_services and handles to subscriptions

### 3.0.0-rc7

-   VFS-2567 Rework space\_record API in space data backend
-   VFS-2607 Fix a bug in group\_logic can\_view\_public\_data
-   VFS-2607 Fix a bug in group\_logic has effective user
-   VFS-2607 Add public group type for groups that users do not have
    view data privs
-   VFS-2607 Allow viewing group details for users that have view privs
    in any space that contains the group
-   VFS-2607 Allow access to public data of groups and spaces for users
    without view privileges
-   VFS-2567 Check view permissions during space find in GUI backend
-   VFS-2607 Introduce env variable deciding about automatic first space
    creation
-   VFS-2567 Add verification of view perms in provider GUI backend
-   VFS-2607 Add better error reporting in REST user PATCH
-   VFS-2607 Add OIDC integration with EGI
-   VFS-2607 Fix onepanel users not being added to global groups
-   VFS-2548 Disable global groups by default
-   VFS-2548 Fix bugs in space name mapping mechanism, add CT tests
    verifying automatic space membership by global grups
-   VFS-2548 Add overwrite argument to set\_space\_name\_mapping
    function
-   VFS-2548 Add mechanism for automatic groups in OZ
-   VFS-2469 Add modifying handles via proxy.
-   VFS-2469 Add registering and unregistering of handles via proxy.
-   VFS-2469 Update cluster\_worker and cluster\_manager, add loading of
    gen\_server2 to relx.
-   VFS-2405 Treat share as different record than space, create
    corresponding modules
-   VFS-2405 Update share redirection path to provider
-   VFS-2407 Use separate record for shares rather then space
-   VFS-2469 Reorganize rest\_modules\_test\_SUITE and add empty test
    mocks for handles management.
-   VFS-2397 Implement oai\_handler

### 3.0.0-rc6

-   VFS-2582 Using GUI fix for blank notifications
-   VFS-2390 Upgrade rebar to version 3

### 3.0.0-rc5

-   VFS-2491 Add RPC call to join a space
-   VFS-2468 Add log level opt to location service script
-   VFS-2468 Add nodejs to packages requirements
-   VFS-2500 described location service config options
-   VFS-2309 no more bootstrap on testmaster
-   VFS-2309 adjusted location service to use node 4
-   VFS-2176 all \'A\' DNS entries used as bootstrap nodes
-   VFS-2309 boostrap location service client on testmaster
-   VFS-2309 rest auth skeleton
-   VFS-2309 public keys retrieved from directly publishers
-   VFS-2309 refactoring: encoded key pased & utitlities separated
-   VFS-2309 identity verification in oz
-   VFS-2309 location service added

### 3.0.0-rc4

-   VFS-2156 Change onepanel config env variables types
-   Disable openid providers in auth.config by default, leaving
    basicAuth only

### 3.0.0-RC3

-   VFS-2156 Remove GUI files
-   VFS-2436 Allow modifying user alias via REST
-   VFS-2436 Disallow adding inexistent users or groups to spaces via
    REST admin endpoint
-   VFS-2154 Fix a mixup between privileges expressed in atoms and
    binaries in rest privileges tests
-   VFS-2154 Update privileges rest handler to accept patch rather than
    put
-   VFS-2154 Use all possible combinations of privs rather than random
    subset
-   VFS-2154 Major refactor of rest privileges test suite and some
    improvements to the tested code
-   VFS-2154 Further refinement of OZ API privileges tests, update
    oz-gui-default reference
-   VFS-2358 HOTFIX: Fix a badmatch in provider data backend
-   VFS-2358 Update comments concerning module APIs in space and
    provider data backends
-   Releasing new version 3.0.0-RC2
-   VFS-2154 Rework tests for OZ API, fix some bugs that were revealed
    by tests
-   VFS-2273 Handle macaroon verification errors
-   VFS-2358 Implement unsupport space functionality in GUI
-   VFS-2357 Add checks if taking away privileges works in OZ API REST
-   VFS-2269 Enable Symmetric Multiprocessing
-   VFS-2357 Add admin endpoints to add/remove users and groups from
    spaces, fix a couple of bugs
-   VFS-2359 Turn off HSTS by default, allow configuration via
    app.config, improve docs integration
-   VFS-2359 Add handler for serving static docs files located on
    another server
-   Releasing new version 3.0.0-RC1
-   VFS-2250 Use wrappers for macaroon serialization

### 3.0.0-RC2

-   VFS-2357 Add checks if taking away privileges works in OZ API REST
-   VFS-2269 Enable Symmetric Multiprocessing
-   VFS-2357 Add integration tests for OZ API REST functionalities
-   VFS-2357 Add admin endpoints to add/remove users and groups from
    spaces, fix a couple of bugs
-   VFS-2351 onedata\_auth is persistent
-   VFS-2359 Turn off HSTS by default, allow configuration via
    app.config, improve docs integration
-   VFS-2359 Add handler for serving static docs files located on
    another server

### 3.0.0-RC1

-   VFS-2316 Update etls.
-   VFS-2250 Use wrappers for macaroon serialization
-   VFS-2121 Tests refactoring

### 3.0.0-beta8

-   minor changes and improvements

### 3.0.0-beta7

-   VFS-2225 Update GUI docker image
-   bugfix - cleaning changed users list

### 3.0.0-beta6

-   Update erlang tls
-   VFS-2133 Rework google and indigo auth logic
-   VFS-2111 Integrate user management with onepanel
-   VFS-2111 Enable changing password only if user has basic auth
    enabled
-   VFS-2111 Add several funcitons to admin API - list spaces, list
    providers, list providers of space
-   VFS-2111 Add rest handler for OZ API privileges manipulation
-   Fix space name mapping after space removal
-   VFS-2111 Implement basic login backend
-   VFS-2111 allow basic auth in all user requests
-   VFS-2111 Allow using client token for authorization in REST,
    provider certs are no longer obligatory when not required, add REST
    API to get client\_token
-   VFS-2111 Change values returned from rpc\_backends to JSON objects
    rather than strings
-   VFS-2087 supported spaces are not public
-   VFS-2087 push provider updates to all

### 3.0.0-beta5

-   VFS-2068 adjust to new webscoket adapter API
-   VFS-1987 concurrent refreshes and updates test
-   VFS-1987 subscriptions updates with nested groups of users groups
-   VFS-1987 set & get for nested group privileges
-   VFS-1987 get effective user in rest
-   VFS-1987 nested groups in global config (json)
-   VFS-1987 nested groups rest privileges
-   VFS-1987 nested groups in subscriptions
-   VFS-1987 effective groups in user document
-   VFS-1987 general graph traversal
-   VFS-1987 effective users in logic
-   VFS-1987 model changes

### 3.0.0-beta4

-   Minor updates.

### 3.0.0-beta3

-   VFS-1860 explicit default space added
-   VFS-1860 trimmed (with public data only) users are always pushed
-   VFS-1825 rework gui starting in zone up
-   VFS-1768: Do not allow provider drop above onezone modals
-   VFS-1768: Do not allow scroll bars on atlas
-   VFS-1607 Add space canonical name to get data response in user
    context.
-   VFS-1596 Update getting onedata user.
-   VFS-1607 Save space name mapping in user document.
-   VFS-1596 More detailed get\_data for user.
-   VFS-1768: Spinners in login boxes
-   VFS-1770 fix disappearing client tokens

### 3.0.0-beta1

-   VFS-1770 make sure provider is inoperable before displaying it
-   VFS-1768: Token copy button in modals; dynamic page titles
-   VFS-1521 remove providerId restriction on new tokens
-   VFS-1757 Change application ports availability checking procedure.
-   VFS-1792 moving privilages to ctool
-   VFS-1796 location as optional create args
-   VFS-1629 covered cache malfunctions
-   VFS-1629 covered fetching old changes from db
-   VFS-1629 limiting db fetch life
-   VFS-1629 connected provider to the OZ (over websocket)
-   VFS-1629 subscribtions over websocket
-   VFS-1629 sending info needed by op
-   VFS-1629 user subscriptions
-   VFS-1629 client subscriptions
-   VFS-1629 extracted outbox
-   VFS-1629 buffering outbox
-   VFS-1629 extraction of subscription handling
-   VFS-1629 introduction of modules: cache, subscribers, translator
-   VFS-1629 provider can obtain only spaces on his own
-   VFS-1629 subscriptions via rest

### 3.0.0-alpha3

-   VFS-1638 enable simple auth mixins and adjust redirection pages
    after login
-   VFS-1672: New callbacks for login create
-   VFS-1672: Conditionally show/hide modals on onezone
-   VFS-1638 add support for custom GUI
-   VFS-1672: User dropdown on the right
-   VFS-1672: New homepage account dropdown style
-   VFS-1672: New rendering of social icons in onezone
-   VFS-1638 add real user credentials to backend
-   VFS-1672: Onezone panels
-   VFS-1638 differentiate between chosen provider and default provider
-   VFS-1672: Updated oneicons 1.3
-   VFS-1665 Pull in Macaroons.
-   VFS-1544 distributed gr is packaged via onezone repo
-   VFS-1672: Fixed main menu blinking on click
-   VFS-1544 uuids easier to use with http
-   VFS-1638 switch to hash based location service, use server backend
    for spaces and providers
-   VFS-1638 enable login for all oauth providers
-   VFS-1638 allow logging in with openid providers
-   VFS-1638 allow pages without .html extension to server index.html
-   VFS-1638 add new gui\_livereload modes
-   VFS-1544 updating CW and adapting to refactored dns
-   VFS-1672: New routes and draft of onezone layout
-   VFS-1636: Red menu highlight on top (z dimension) of menu line
-   VFS-1638 add polling and watching options for gui livereload

### 3.0.0-alpha2

-   VFS-1665 Pull in Macaroons.

### 3.0.0-alpha

-   VFS-1622 Add openssl to package requirements.
-   VFS-1520-delete annotations from all ct\_tests
-   VFS-1528 Remove deprecated use of erlang:now/0
-   VFS-1428 Add endpoint that allows for getting token issuer.
-   VFS-1378 adjust to the new ctool API
-   VFS-1223 add rest port to macaroon\'s location
-   VFS-1223 Handle empty macaroon-discharges header.
-   VFS-1223 Parse macaroons from HTTP headers.
-   VFS-1223 Implement first revision of macaroon-based auth.
-   VFS-1123 Use Macaroons for tokens.
-   itegrate ssl2
-   VFS-914, DNS server in GR supports aliases
-   VFS-914, add DNS to GR

### 2.1.0

-   Better behaviour when GUI window is small
-   Provider instruction updated

### 2.0.0

-   Support for spaces
-   Support for logging with Google, Facebook, Dropbox, Github and
    PL-Grid
-   User account management enabled
-   Support for tokens

------------------------------------------------------------------------

Generated by sr-release.
