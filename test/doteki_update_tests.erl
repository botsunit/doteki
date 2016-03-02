-module(doteki_update_tests).
-export([function1/1, function2/1]).
-export([function3/1, function4/1]).

-include_lib("eunit/include/eunit.hrl").
-define(CONF0, [
                {app, [
                       {key1, value1},
                       {key2, [
                               {key21, {'fun.doteki_update_tests:function1/1', [123]}},
                               {key22, 'env.MY_CUSTOM_ENV_VAR1'},
                               {key23, value23}
                              ]},
                       {key3, {'fun.doteki_update_tests:function2/1', [123]}},
                       {key4, 'env.MY_CUSTOM_ENV_VAR2'}
                      ]}
               ]).

-define(CONF1, [
                {app, [
                       {key1, value1},
                       {key2, [
                               {key21, {'fun.doteki_update_tests:function3/1', [123]}},
                               {key22, 'env.MY_CUSTOM_ENV_VAR3'},
                               {key23, value23}
                              ]},
                       {key3, {'fun.doteki_update_tests:function4/1', [123]}},
                       {key4, 'env.MY_CUSTOM_ENV_VAR4'}
                      ]}
               ]).

-define(CONF2, [
                {app, [
                       {key2, [
                               {key21, {'fun.doteki_update_tests:function3/1', [123]}},
                               {key22, 'env.MY_CUSTOM_ENV_VAR5'}
                              ]},
                       {key3, {'fun.doteki_update_tests:function4/1', [123]}},
                       {key4, 'env.MY_CUSTOM_ENV_VAR6'}
                      ]}
               ]).

function1(X) -> X + 1.
function2(X) -> X + 2.
function3(X) -> X + 3.
function4(X) -> X + 4.

doteki_update_test_() ->
   [
    fun() ->
        ?assertMatch(ok, doteki:set_env_from_config(?CONF0))
    end
    , fun() ->
        ?assertEqual(value1, doteki:get_env([app, key1])),
        ?assertEqual(124, doteki:get_env([app, key2, key21])),
        ?assertEqual(undefined, doteki:get_env([app, key2, key22])),
        os:putenv("MY_CUSTOM_ENV_VAR1", "\"value22\""),
        ?assertEqual("value22", doteki:get_env([app, key2, key22])),
        ?assertEqual(value23, doteki:get_env([app, key2, key23])),
        ?assertEqual(125, doteki:get_env([app, key3])),
        ?assertEqual(undefined, doteki:get_env([app, key4])),
        os:putenv("MY_CUSTOM_ENV_VAR2", "\"value4\""),
        ?assertEqual("value4", doteki:get_env([app, key4]))
    end
    , fun() ->
        ?assertMatch(ok, doteki:set_env_from_config(?CONF1))
    end
    , fun() ->
        ?assertEqual(value1, doteki:get_env([app, key1])),
        ?assertEqual(126, doteki:get_env([app, key2, key21])),
        ?assertEqual(undefined, doteki:get_env([app, key2, key22])),
        os:putenv("MY_CUSTOM_ENV_VAR3", "\"value22\""),
        ?assertEqual("value22", doteki:get_env([app, key2, key22])),
        ?assertEqual(value23, doteki:get_env([app, key2, key23])),
        ?assertEqual(127, doteki:get_env([app, key3])),
        ?assertEqual(undefined, doteki:get_env([app, key4])),
        os:putenv("MY_CUSTOM_ENV_VAR4", "\"value4\""),
        ?assertEqual("value4", doteki:get_env([app, key4]))
    end
    , fun() ->
        ?assertMatch(ok, doteki:set_env_from_config(?CONF2))
    end
    , fun() ->
        ?assertEqual(undefined, doteki:get_env([app, key1])),
        ?assertEqual(126, doteki:get_env([app, key2, key21])),
        ?assertEqual(undefined, doteki:get_env([app, key2, key22])),
        os:putenv("MY_CUSTOM_ENV_VAR5", "\"value22\""),
        ?assertEqual("value22", doteki:get_env([app, key2, key22])),
        ?assertEqual(undefined, doteki:get_env([app, key2, key23])),
        ?assertEqual(127, doteki:get_env([app, key3])),
        ?assertEqual(undefined, doteki:get_env([app, key4])),
        os:putenv("MY_CUSTOM_ENV_VAR6", "\"value4\""),
        ?assertEqual("value4", doteki:get_env([app, key4]))
    end
   ].

