-module(doteki_rebuild_tests).
-export([function1/1, function2/1]).

-include_lib("eunit/include/eunit.hrl").
-define(CONF, [
               {appcomp, [
                      {key1, value1},
                      {key2, [
                              {key21, {'fun.doteki_rebuild_tests:function1/1', [123]}},
                              {key22, 'env.MY_REBUILD_TESTS_ENV_VAR1'},
                              {key23, value23}
                             ]},
                      {key3, {'fun.doteki_rebuild_tests:function2/1', [123]}},
                      {key4, 'env.MY_REBUILD_TESTS_ENV_VAR2'},
                      {env, {system, "HOME"}}
                     ]}
              ]).


function1(X) -> X + 1.
function2(X) -> X + 2.

doteki_update_test_() ->
   [
    fun() ->
        ?assertMatch(ok, doteki:set_env_from_config(?CONF))
    end
    , fun() ->
        ?assertEqual(value1, doteki:get_env([appcomp, key1])),
        ?assertEqual(124, doteki:get_env([appcomp, key2, key21])),
        ?assertEqual(undefined, doteki:get_env([appcomp, key2, key22])),
        os:putenv("MY_REBUILD_TESTS_ENV_VAR1", "\"value22\""),
        ?assertEqual("value22", doteki:get_env([appcomp, key2, key22])),
        ?assertEqual(value23, doteki:get_env([appcomp, key2, key23])),
        ?assertEqual(125, doteki:get_env([appcomp, key3])),
        ?assertEqual(undefined, doteki:get_env([appcomp, key4])),
        os:putenv("MY_REBUILD_TESTS_ENV_VAR2", "\"value4\""),
        ?assertEqual("value4", doteki:get_env([appcomp, key4])),
        ?assertEqual(os:getenv("HOME"), doteki:get_env([appcomp, env]))
    end
    , fun() ->
        os:putenv("MY_REBUILD_TESTS_ENV_VAR1", "compiled1"),
        os:putenv("MY_REBUILD_TESTS_ENV_VAR2", "compiled2"),
        os:putenv("APPCOMP_KEY1", "value_set_by_env:atom"),
        os:putenv("APPCOMP_KEY2_KEY23", "second_value_set_by_env:binary"),
        ?assertEqual(ok, doteki:compile(appcomp)),
        os:putenv("MY_REBUILD_TESTS_ENV_VAR1", "error"),
        os:putenv("MY_REBUILD_TESTS_ENV_VAR2", "error"),
        ?assertEqual(compiled1, doteki:get_env([appcomp, key2, key22])),
        ?assertEqual(value_set_by_env, doteki:get_env([appcomp, key1])),
        ?assertEqual(125, doteki:get_env([appcomp, key3])),
        ?assertEqual(compiled2, doteki:get_env([appcomp, key4])),
        ?assertEqual({ok, 125}, application:get_env(appcomp, key3)),
        ?assertEqual({ok, compiled2}, application:get_env(appcomp, key4)),
        ?assertEqual({ok, value_set_by_env}, application:get_env(appcomp, key1)),
        AllEnv = doteki:get_all_env(appcomp),
        lists:foreach(fun({K, _} = V) ->
                          ?assertMatch(V, lists:keyfind(K, 1, AllEnv))
                      end, [{key1, value_set_by_env},
                            {key3, 125},
                            {key4, compiled2}]),
        {key2, AllKey2} = lists:keyfind(key2, 1, AllEnv),
        lists:foreach(fun({K, _} = V) ->
                          ?assertMatch(V, lists:keyfind(K, 1, AllKey2))
                      end, [{key21,124},
                            {key22,compiled1},
                            {key23,<<"second_value_set_by_env">>}]),
        ?assertEqual(os:getenv("HOME"), doteki:get_env([appcomp, env]))
    end
   ].

