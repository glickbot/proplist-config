.PHONY: clean dialyzer_warnings xref_warnings deps test

all: deps compile
	./rebar skip_deps=true escriptize

deps:
	./rebar get-deps

compile:
	./rebar compile

docs:
	./rebar skip_deps=true doc

clean:
	@./rebar clean

distclean: clean
	@rm -rf deps
