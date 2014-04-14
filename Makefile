.PHONY: test deps

all: deps

deps:
	@./rebar get-deps

compile:
	@./rebar compile

clean:
	@./rebar clean