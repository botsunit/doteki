-module(doteki_default_tests).

-include_lib("eunit/include/eunit.hrl").
-define(CONF, [
               {app1, [
                       {app1key1, app1value1}
                      ]},
               {app2, [
                       {app2key1, app2value1}
                      ]}
              ]).

doteki_default_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_default())
    , ?_test(t_default_ignore())
    , ?_test(t_default_setenv())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_default() ->
  ?assertMatch(ok, doteki:set_env_from_config(?CONF)),
  ?assertEqual(missing1atom, doteki:get_env([app3, app3missing1], missing1atom)),
  ?assertEqual(<<"missing1bin">>, doteki:get_env([app3, app3missing1], <<"missing1bin">>)),
  ?assertEqual({missing, 1, "tuple"}, doteki:get_env([app3, app3missing1], {missing, 1, "tuple"})),
  ?assertEqual("missing1str", doteki:get_env([app3, app3missing1], "missing1str")).

t_default_ignore() ->
  ?assertMatch(ok, doteki:set_env_from_config(?CONF)),
  ?assertEqual(app1value1, doteki:get_env([app1, app1key1], missing1atom)),
  ?assertEqual(app2value1, doteki:get_env([app2, app2key1], missing2atom)).

t_default_setenv() ->
  ?assertMatch(ok, doteki:set_env_from_config(?CONF)),
  os:putenv("APP3_APP3MISSING1", "env1atom"),
  ?assertEqual(env1atom, doteki:get_env([app3, app3missing1], missing1atom)),
  os:putenv("APP3_APP3MISSING1", "<<\"env1bin\">>"),
  ?assertEqual(<<"env1bin">>, doteki:get_env([app3, app3missing1], <<"missing1bin">>)),
  os:putenv("APP3_APP3MISSING1", "{env, 1, \"tuple\"}"),
  ?assertEqual({env, 1, "tuple"}, doteki:get_env([app3, app3missing1], {missing, 1, "tuple"})),
  os:putenv("APP3_APP3MISSING1", "\"env1str\""),
  ?assertEqual("env1str", doteki:get_env([app3, app3missing1], "missing1str")).

