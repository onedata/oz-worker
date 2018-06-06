.EXPORT_ALL_VARIABLES:

REPO	        ?= oz_worker

# distro for package building (oneof: wily, fedora-23-x86_64)
DISTRIBUTION    ?= none
export DISTRIBUTION

PKG_REVISION    ?= $(shell git describe --tags --always)
PKG_VERSION     ?= $(shell git describe --tags --always | tr - .)
PKG_ID           = oz_worker-$(PKG_VERSION)
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar3
LIB_DIR          = _build/default/lib
REL_DIRS         = _build/default/rel
PKG_VARS_CONFIG  = pkg.vars.config
OVERLAY_VARS    ?= --overlay_vars=rel/vars.config

DOCKER_REG_USER        ?= ""
DOCKER_REG_PASSWORD    ?= ""
DOCKER_REG_EMAIL       ?= ""

BASE_DIR         = $(shell pwd)
GIT_URL := $(shell git config --get remote.origin.url | sed -e 's/\(\/[^/]*\)$$//g')
GIT_URL := $(shell if [ "${GIT_URL}" = "file:/" ]; then echo 'ssh://git@git.plgrid.pl:7999/vfs'; else echo ${GIT_URL}; fi)
ONEDATA_GIT_URL := $(shell if [ "${ONEDATA_GIT_URL}" = "" ]; then echo ${GIT_URL}; else echo ${ONEDATA_GIT_URL}; fi)
export ONEDATA_GIT_URL

BUILD_VERSION := $(subst $(shell git describe --tags --abbrev=0)-,,$(shell git describe --tags --long))

.PHONY: test deps upgrade generate package

all: test_rel

##
## Rebar targets
##

upgrade:
	$(REBAR) upgrade

deps:
	$(LIB_DIR)/gui/pull-gui.sh gui-config.sh

compile:
	$(REBAR) compile

## Generates a production release
generate: template compile deps
	$(REBAR) release $(OVERLAY_VARS)

clean:
	$(REBAR) clean

distclean: clean
	$(REBAR) clean --all

template:
	sed "s/{build_version, \".*\"}/{build_version, \"${BUILD_VERSION}\"}/" ./rel/vars.config.template > ./rel/vars.config

##
## Submodules
##

submodules:
	git submodule sync --recursive ${submodule}
	git submodule update --init --recursive ${submodule}

##
## Release targets
##

rel: generate

test_rel: rel cm_rel

cm_rel:
	mkdir -p cluster_manager/bamboos/gen_dev
	make -C $(LIB_DIR)/cluster_manager/ submodules
	cp -rf $(LIB_DIR)/cluster_manager/bamboos/gen_dev cluster_manager/bamboos
	printf "\n{base_dir, \"$(BASE_DIR)/cluster_manager/_build\"}." >> $(LIB_DIR)/cluster_manager/rebar.config
	make -C $(LIB_DIR)/cluster_manager/ rel
	sed -i "s@{base_dir, \"$(PWD)/cluster_manager/_build\"}\.@@" $(LIB_DIR)/cluster_manager/rebar.config

relclean:
	rm -rf _build/rel/oz_worker
	rm -rf cluster_manager/_build/rel/cluster_manager

##
## Testing
##

eunit:
	$(REBAR) do eunit skip_deps=true --suite=${SUITES}, cover
## Rename all tests in order to remove duplicated names (add _(++i) suffix to each test)
	@for tout in `find test -name "TEST-*.xml"`; do awk '/testcase/{gsub("_[0-9]+\"", "_" ++i "\"")}1' $$tout > $$tout.tmp; mv $$tout.tmp $$tout; done

coverage:
	$(BASE_DIR)/bamboos/docker/coverage.escript $(BASE_DIR) $(on_bamboo)

##
## Dialyzer targets local
##

# Dialyzes the project.
dialyzer:
	$(REBAR) dialyzer

##
## Packaging targets
##

check_distribution:
ifeq ($(DISTRIBUTION), none)
	@echo "Please provide package distribution. Oneof: 'xenial', 'centos-7-x86_64'"
	@exit 1
else
	@echo "Building package for distribution $(DISTRIBUTION)"
endif

package/$(PKG_ID).tar.gz:
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION) | (cd package && tar -xf -)
	git submodule foreach "git archive --prefix=$(PKG_ID)/\$$path/ \$$sha1 | (cd \$$toplevel/package && tar -xf -)"
	${MAKE} -C package/$(PKG_ID) upgrade deps
	for dep in package/$(PKG_ID) package/$(PKG_ID)/$(LIB_DIR)/*; do \
	     echo "Processing dependency: `basename $${dep}`"; \
	     vsn=`git --git-dir=$${dep}/.git describe --tags 2>/dev/null`; \
	     mkdir -p $${dep}/priv; \
	     echo "$${vsn}" > $${dep}/priv/vsn.git; \
	     sed -i'' "s/{vsn,\\s*git}/{vsn, \"$${vsn}\"}/" $${dep}/src/*.app.src 2>/dev/null || true; \
	done
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

dist: package/$(PKG_ID).tar.gz
	cp package/$(PKG_ID).tar.gz .

package: check_distribution package/$(PKG_ID).tar.gz
	${MAKE} -C package -f $(PKG_ID)/node_package/Makefile

pkgclean:
	rm -rf package
