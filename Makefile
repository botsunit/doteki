PROJECT = doteki

DOC_DEPS = edown
DEPS = bucs
dep_edown = git https://github.com/botsunit/edown.git master
dep_bucs = git https://github.com/botsunit/bucs.git master

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

