# oz-worker

*oz-worker* is a component of [Onedata](http://onedata.org) distributed data management platform, which serves as a worker process of [Onezone](https://github.com/onedata/onezone) service. Onezone service requires at least one *oz-worker* instance.  Adding more *oz-worker* nodes scales the cluster allowing for processing more requests in parallel. *oz-worker* instances are coordinated by [cluster-manager](https://github.com/onedata/cluster-manager) process, which should be deployed at least in one instance per entire cluster. Adding *cluster-manager* nodes increases fault tolerance of the Onezone service.

*oz-worker* is a specialization of generic Onedata worker process [cluster-worker](https://github.com/onedata/cluster-worker). The *cluster-worker* provides generic funcitonalities such as persistence and cluster management, while *oz-worker* augments it with its specific logic.

The main objective of *oz-worker* is to provide logic for coordinating the *Oneprovider* instances. Onezone acts as an intermediary in a network of cooperating *Oneproviders*. It stores metadata of system entites, i.e. users, groups, spaces and providers, manages relations between them and informs all *oneprovider*s about any changes that are in their region of interest.


# Using

*oz-worker* is an internal component of Onezone service and it should only be started as part of its deployment.

## Dependencies

* docker client > 1.10
* python >= 2.7


## Building
To build *oz-worker* make sure submodules are initialized and use the provided build script:
```
make submodules
./make.py
```

## Configuration and Running
*oz-worker* can be started using [bamboo](https://github.com/onedata/bamboo) scripts that are included in the repository. From the root of *oz-worker* project, run:

```
./bamboos/docker/zone_up.py bamboos/example_env/example_env.json
```

As *oz-worker* won't work without *cluster_manager*, both those applications will be started and connected into a small cluster. The section "zone_domains" in the JSON file defines all instances of onezone clusters that should be started and allows for basic configuration.

After the script has finished, you should have a running, dockerized onezone instance. Enter the graphical user interface on https://<docker-ip>.


# APIs

*oz-worker* has an exhaustive REST API for *Oneprovider*s. Requests are authorized based on *oneprovider* certificates that are sent with every request. The API allows for CRUD operations on users, spaces, groups and providers. The matching client in Erlang is included in *ctool* repository, and is directly used by *oneprovider*s.
