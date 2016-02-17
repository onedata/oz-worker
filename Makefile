.PHONY: test deps generate

DOCKER_REG_USER     ?= ""
DOCKER_REG_PASSWORD ?= ""
DOCKER_REG_EMAIL    ?= ""

BASE_DIR         = $(shell pwd)
GIT_URL         := $(shell git config --get remote.origin.url | sed -e 's/\(\/[^/]*\)$$//g')
GIT_URL         := $(shell if [ "${GIT_URL}" = "file:/" ]; then echo 'ssh://git@git.plgrid.pl:7999/vfs'; else echo ${GIT_URL}; fi)
ONEDATA_GIT_URL := $(shell if [ "${ONEDATA_GIT_URL}" = "" ]; then echo ${GIT_URL}; else echo ${ONEDATA_GIT_URL}; fi)
export ONEDATA_GIT_URL

all: rel

deps:
	@./rebar get-deps
	@git submodule init
	@git submodule update

compile:
	@./rebar compile

generate: deps compile
	@./rebar generate

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

coverage:
	$(BASE_DIR)/bamboos/docker/coverage.escript $(BASE_DIR)

##
## Release targets
##

rel: generate

relclean:
	rm -rf rel/globalregistry

rpmdirs:
	rm -rf package
	mkdir -p package/fedora-23-x86_64/x86_64

package: rel rpmdirs
	make -C onepanel rel CONFIG=config/globalregistry.config
	./rel/rpm/create_rpm
	mv rel/globalregistry-*.rpm package/fedora-23-x86_64/x86_64
	tar -czf package.tar.gz package

docker:
	./dockerbuild.py --user $(DOCKER_REG_USER) --password $(DOCKER_REG_PASSWORD) \
                         --email $(DOCKER_REG_EMAIL) --name globalregistry --publish --remove packaging
