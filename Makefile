.PHONY: doc
FIND_REBAR = \
                REBAR_BIN=; \
                for x in ./rebar3 rebar3; do \
                if type "$${x%% *}" >/dev/null 2>/dev/null; then REBAR_BIN=$$x; break; fi; \
                done; \
                if [ -z "$$REBAR_BIN" ]; then echo 1>&2 "Unable to find rebar3"; exit 2; fi
REBAR = $(FIND_REBAR); $$REBAR_BIN
MIX = mix

compile-erl:
	@$(REBAR) compile

compile-ex: elixir
	@$(MIX) deps.get
	@$(MIX) compile

elixir:
	@$(REBAR) elixir generate_mix
	@$(REBAR) elixir generate_lib

tests:
	@$(REBAR) eunit

doc:
	@$(REBAR) as doc edoc

dist: dist-ex dist-erl doc

release: dist-ex dist-erl
	@$(REBAR) hex publish

dist-erl: distclean-erl compile-erl tests

distclean-erl: distclean
	@rm -f rebar.lock

dist-ex: distclean-ex compile-ex

distclean-ex: distclean
	@rm -f mix.lock

distclean:
	@rm -rf _build test/eunit deps ebin

