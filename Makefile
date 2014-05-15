REPO			?= globalregistry

PKG_REVISION    ?= $(shell git describe --tags --always)
PKG_VERSION		?= $(shell git describe --tags --always | tr - .)
PKG_ID           = globalregistry-$(PKG_VERSION)
PKG_BUILD        = 1
BASE_DIR         = $(shell pwd)
ERLANG_BIN       = $(shell dirname $(shell which erl))
REBAR           ?= $(BASE_DIR)/rebar
OVERLAY_VARS    ?=

.PHONY: test deps generate

all: deps compile

deps:
	@./rebar get-deps

compile:
	@./rebar compile

generate:
	@./rebar generate $(OVERLAY_VARS)

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps
	@rm -rf $(PKG_ID).tar.gz

##
## Testing
##
test: deps compile
	@./rebar skip_deps=true eunit

ct: deps compile
	@./test_distributed/start_distributed_test.sh

##
## Release targets
##
rel: deps compile generate

relclean:
	rm -rf rel/globalregistry

##
## Package generation
## (see http://www.erlang-factory.com/upload/presentations/857/JaredEUC-2013.pdf)

.PHONY: package
export PKG_VERSION PKG_ID PKG_BUILD BASE_DIR ERLANG_BIN REBAR OVERLAY_VARS RELEASE

package.src: deps
	mkdir -p package
	rm -rf package/$(PKG_ID)
	git archive --format=tar --prefix=$(PKG_ID)/ $(PKG_REVISION)| (cd package && tar -xf -)
	${MAKE} -C package/$(PKG_ID) deps
	mkdir -p package/$(PKG_ID)/priv
	git --git-dir=.git describe --tags --always >package/$(PKG_ID)/priv/vsn.git
	for dep in package/$(PKG_ID)/deps/*; do \
             echo "Processing dep: $${dep}"; \
             mkdir -p $${dep}/priv; \
             git --git-dir=$${dep}/.git describe --tags --always >$${dep}/priv/vsn.git; \
        done
	find package/$(PKG_ID) -depth -name ".git" -exec rm -rf {} \;
	tar -C package -czf package/$(PKG_ID).tar.gz $(PKG_ID)

dist: package.src
	cp package/$(PKG_ID).tar.gz .

package: package.src
	make -C package -f $(PKG_ID)/deps/node_package/Makefile

pkgclean: distclean
	rm -rf package
