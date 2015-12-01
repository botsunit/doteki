-module(doteki_tests).

-include_lib("eunit/include/eunit.hrl").
-define(CONF, [
               {app1, [
                       {app1key1, app1value1}, 
                       {app1key2, [
                                   {app1key21, app1value21}, 
                                   {app1key22, app1value22}
                                  ]}
                      ]},
               {app2, [
                       {app2key1, app2value1}, 
                       {app2key2, [
                                   {app2key21, 'env.MY_CUSTOM_ENV_VAR'}, 
                                   {app2key22, app2value22}
                                  ]}
                      ]}
              ]).

doteki_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_default())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_default() ->
  ?assertMatch(ok, doteki:set_env_from_config(?CONF)),
  ?assertEqual(app2value22, doteki:get_env([app2, app2key2, app2key22])),
  os:putenv("APP2_APP2KEY2_APP2KEY22", "overloaded_app2value22"),
  ?assertEqual("overloaded_app2value22", doteki:get_env([app2, app2key2, app2key22])),
  ?assertEqual(undefined, doteki:get_env([app2, app2key2, app2key21])),
  os:putenv("MY_CUSTOM_ENV_VAR", "forced_app2value21"),
  ?assertEqual("forced_app2value21", doteki:get_env([app2, app2key2, app2key21])).


