PROJECT = doteki

DEP_PLUGINS = mix.mk
BUILD_DEPS = mix.mk
ELIXIR_VERSION = ~> 1.2
dep_mix.mk = git https://github.com/botsunit/mix.mk.git master

DEPS = bucs
dep_bucs = git https://github.com/botsunit/bucs.git 0.0.1

DOC_DEPS = edown
dep_edown = git https://github.com/botsunit/edown.git master

include erlang.mk

EDOC_OPTS = {doclet, edown_doclet} \
						, {app_default, "http://www.erlang.org/doc/man"} \
						, {source_path, ["src"]} \
						, {overview, "overview.edoc"} \
						, {stylesheet, ""} \
						, {image, ""} \
						, {top_level_readme, {"./README.md", "https://github.com/botsunit/${PROJECT}"}}

EUNIT_OPTS = verbose, {report, {eunit_surefire, [{dir, "test"}]}}

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include -config config/doteki.config

release: app mix.exs

