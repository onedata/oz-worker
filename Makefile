.PHONY: test deps generate

BASE_DIR         = $(shell pwd)

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
	        common_test test_server syntax_tools compiler edoc -r deps; \
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

rpm: rel
	make -C onepanel rel CONFIG=config/globalregistry.config
	./rel/rpm/create_rpm
