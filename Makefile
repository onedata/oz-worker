.PHONY: test deps generate

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
## Dialyzer
##

# Builds .dialyzer.plt init file. This is internal target, call dialyzer_init instead
.dialyzer.plt:
	-dialyzer --build_plt --output_plt .dialyzer.plt --apps kernel stdlib sasl erts ssl tools runtime_tools crypto inets xmerl snmp public_key eunit syntax_tools compiler ./deps/*/ebin


# Starts dialyzer on whole ./ebin dir. If .dialyzer.plt does not exist, will be generated
dialyzer: compile .dialyzer.plt
	-dialyzer ./ebin --plt .dialyzer.plt -Werror_handling -Wrace_conditions


# Starts full initialization of .dialyzer.plt that is required by dialyzer
dialyzer_init: compile .dialyzer.plt

##
## Testing
##

test: deps compile
	@./rebar skip_deps=true eunit
	@for tout in `find test -name "TEST-*.xml"`; do awk '/testcase/{gsub("_[0-9]+\"", "_" ++i "\"")}1' $$tout > $$tout.tmp; mv $$tout.tmp $$tout; done


ct: deps compile
	@./test_distributed/start_distributed_test.sh
	@for tout in `find test_distributed/log -name "TEST-report.xml"`; do awk '/testcase/{gsub("<testcase name=\"[a-z]+_per_suite\"(([^/>]*/>)|([^>]*>[^<]*</testcase>))", "")}1' $$tout > $$tout.tmp; mv $$tout.tmp $$tout; done

##
## Release targets
##

rel: generate

relclean:
	rm -rf rel/globalregistry

rpm: rel
	make -C onepanel rel CONFIG=config/globalregistry.config
	./rel/rpm/create_rpm
