.PHONY: test deps generate test_gui

BASE_DIR         = $(shell pwd)
GIT_URL := $(shell git config --get remote.origin.url | sed -e 's/\(\/[^/]*\)$$//g')
GIT_URL := $(shell if [ "${GIT_URL}" = "file:/" ]; then echo 'ssh://git@git.plgrid.pl:7999/vfs'; else echo ${GIT_URL}; fi)
ONEDATA_GIT_URL := $(shell if [ "${ONEDATA_GIT_URL}" = "" ]; then echo ${GIT_URL}; else echo ${ONEDATA_GIT_URL}; fi)
export ONEDATA_GIT_URL

all: test_rel

deps:
	@./rebar get-deps
	@git submodule init
	@git submodule update

gui_dev:
	./deps/gui/build_gui.sh dev

gui_prod:
	./deps/gui/build_gui.sh prod

gui_doc:
	jsdoc -c src/http/gui/.jsdoc.conf src/http/gui/app

compile:
	@./rebar compile

## Generates a dev release
generate_dev: deps compile gui_dev
	# Move gui tmp dir away from sources, so as to prevent
	# rebar from entering it during spec generation and crashing
	mv src/http/gui/tmp /tmp/gui_tmp
	./rebar generate $(OVERLAY_VARS)
	# Bring back the tmp dir to its normal location
	mv /tmp/gui_tmp src/http/gui/tmp

## Generates a production release
generate: deps compile gui_prod
	# Move gui tmp dir away from sources, so as to prevent
	# rebar from entering it during spec generation and crashing
	mv src/http/gui/tmp /tmp/gui_tmp
	./rebar generate $(OVERLAY_VARS)
	# Bring back the tmp dir to its normal location
	mv /tmp/gui_tmp src/http/gui/tmp

clean:
	@./rebar clean

distclean: clean
	@./rebar delete-deps

##
## Dialyzer targets local
##

PLT ?= .dialyzer.plt

# Builds dialyzer's Persistent Lookup Table file.
.PHONY: plt
plt:
	dialyzer --check_plt --plt ${PLT}; \
	if [ $$? != 0 ]; then \
	    dialyzer --build_plt --output_plt ${PLT} --apps kernel stdlib sasl erts \
	        ssl tools runtime_tools crypto inets xmerl snmp public_key eunit \
	        common_test test_server syntax_tools compiler edoc mnesia hipe \
	        ssh webtool -r deps; \
	fi; exit 0


# Dialyzes the project.
dialyzer: plt
	dialyzer ./ebin --plt ${PLT} -Werror_handling -Wrace_conditions --fullpath


##
## Testing
##

eunit:
	./rebar eunit skip_deps=true suites=${SUITES}
## Rename all tests in order to remove duplicated names (add _(++i) suffix to each test)
	@for tout in `find test -name "TEST-*.xml"`; do awk '/testcase/{gsub("_[0-9]+\"", "_" ++i "\"")}1' $$tout > $$tout.tmp; mv $$tout.tmp $$tout; done

test_gui:
	cd test_gui && ember test

coverage:
	$(BASE_DIR)/bamboos/docker/coverage.escript $(BASE_DIR)

##
## Release targets
##

rel: generate

test_rel: generate_dev

relclean:
	rm -rf rel/globalregistry

rpm: rel
	make -C onepanel rel CONFIG=config/globalregistry.config
	./rel/rpm/create_rpm
