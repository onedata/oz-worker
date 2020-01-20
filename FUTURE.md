#### Improvements

* VFS-5901 Application config can now be customized with arbitrary number
  of config files added to /etc/oz_worker/config.d/ directory.

* VFS-5730 Introduced differentiation of named and temporary tokens (applicable 
  to both access and invite tokens). Named tokens are assigned a name and are
  retrievable from the Onezone GUI or REST API. They can be revoked, unrevoked 
  or deleted at will. Temporary tokens are valid for a limited time, cannot be
  retrieved or deleted and share a common secret (can be revoked all at once for
  given subject). Temporary tokens do not have any persistence and can be 
  created in unlimited amounts.

* VFS-5524, VFS-5735 Invite and access tokens can now contain customizable 
  caveats that confine the context in which the token is valid. This concept is
  taken from Google's macaroons, which are in fact used for underlying 
  implementation. Example caveats include: audience, IP address, ASN, 
  geographical region, system's interface.
  
* VFS-5727 REST API for tokens is now available, allowing to create named and
  temporary tokens with caveats and including a wide range of token management
  endpoints.

* VFS-5874 Data access caveats are now supported in tokens. They are treated as 
  special caveats that reduce the available API to a bare minimum required 
  solely for data access. Tokens with such caveats can only be used in 
  Oneclient or Oneprovider's REST & CDMI. Data access caveats include: readonly
  caveat (allowing readonly access), data path caveat (restricting the logical
  file paths that can be accessed) and objectid caveat (restricting the 
  accessible file IDs).
  
* VFS-5733 The concept of "storage", representing the Oneprovider's storage 
  system on which the space data is stored, has been moved to Onezone. From now 
  on, Oneproviders can share information about storage parameters defined by
  Oneprovider admins and use this knowledge in QoS algorithms that manage data
  replicas. 


#### Bugfixes


#### Removals