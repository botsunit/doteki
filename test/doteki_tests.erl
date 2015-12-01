-module(doteki_tests).
-export([function/0, function/1, function_error/0]).

-include_lib("eunit/include/eunit.hrl").
-define(CONF, [
               {app1, [
                       {app1key1, app1value1}, 
                       {app1key2, [
                                   {app1key21, {'fun.doteki_tests:function/1', [123]}}, 
                                   {app1key22, app1value22}
                                  ]},
                       {app1key3, {'fun.missing:function/1', [123]}}, 
                       {app1key4, {'fun.missing:function/2', [123]}}, 
                       {app1key5, {'fun.missing:function/1', [123, 456]}}, 
                       {app1key6, 'fun.missing:function/0'}, 
                       {app1key7, 'fun.doteki_tests:function_error/0'} 
                      ]},
               {app2, [
                       {app2key1, app2value1}, 
                       {app2key2, [
                                   {app2key21, 'env.MY_CUSTOM_ENV_VAR'}, 
                                   {app2key22, 'fun.doteki_tests:function/0'}
                                  ]}
                      ]}
              ]).

doteki_test_() ->
  {setup,
   fun setup/0, fun teardown/1,
   [
    ?_test(t_default())
    , ?_test(t_envvar())
    , ?_test(f_fun())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_default() ->
  ?assertMatch(ok, doteki:set_env_from_config(?CONF)),
  ?assertEqual(app1value22, doteki:get_env([app1, app1key2, app1key22])).

t_envvar() ->
  os:putenv("APP1_APP1KEY2_APP1KEY22", "overloaded_app1value22"),
  ?assertEqual("overloaded_app1value22", doteki:get_env([app1, app1key2, app1key22])),
  
  ?assertEqual(undefined, doteki:get_env([app2, app2key2, app2key21])),
  os:putenv("MY_CUSTOM_ENV_VAR", "forced_app2value21"),
  ?assertEqual("forced_app2value21", doteki:get_env([app2, app2key2, app2key21])).

f_fun() ->
  ?assertEqual(123, doteki:get_env([app2, app2key2, app2key22])),
  ?assertEqual(246, doteki:get_env([app1, app1key2, app1key21])),
  ?assertEqual(missing_function, doteki:get_env([app1, app1key3], missing_function)),
  ?assertEqual(bad_arity, doteki:get_env([app1, app1key4], bad_arity)),
  ?assertEqual(bad_parameters, doteki:get_env([app1, app1key5], bad_parameters)),
  ?assertEqual(missing_function, doteki:get_env([app1, app1key6], missing_function)),
  ?assertEqual(function_error, doteki:get_env([app1, app1key7], function_error)).

function() -> 123.
function(A) -> A * 2.
function_error() -> missing:function().

