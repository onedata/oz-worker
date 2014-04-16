.PHONY: test deps

all: deps compile

deps:
	@./rebar get-deps

compile:
	@./rebar compile

generate:
	@./rebar generate

clean:
	@./rebar clean