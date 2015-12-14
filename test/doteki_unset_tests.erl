-module(doteki_unset_tests).
-export([function/0, function/1, function_error/0]).

-include_lib("eunit/include/eunit.hrl").
-define(CONF, [
               {unsetapp1, [
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
               {unsetapp2, [
                            {app2key1, app2value1},
                            {app2key2, [
                                        {app2key21, 'env.MY_CUSTOM_UNSET_ENV_VAR'},
                                        {app2key22, 'fun.doteki_tests:function/0'}
                                       ]}
                           ]}
              ]).

doteki_unset_test_() ->
  [
   fun() ->
       ?assertMatch(ok, doteki:set_env_from_config(?CONF))
   end
   , fun() ->
       ?assertEqual(app1value1, doteki:get_env([unsetapp1, app1key1])),
       ?assertEqual(123*2, doteki:get_env([unsetapp1, app1key2, app1key21])),
       ?assertEqual(app1value22, doteki:get_env([unsetapp1, app1key2, app1key22])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key3])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key4])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key5])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key6])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key7])),
       ?assertEqual(app2value1, doteki:get_env([unsetapp2, app2key1])),
       ?assertEqual(undefined, doteki:get_env([unsetapp2, app2key2, app2key21])),
       ?assertEqual(123, doteki:get_env([unsetapp2, app2key2, app2key22]))
   end
   , fun() ->
       ok = doteki:unset_env([unsetapp1, app1key1]),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key1])),
       ?assertEqual(123*2, doteki:get_env([unsetapp1, app1key2, app1key21])),
       ?assertEqual(app1value22, doteki:get_env([unsetapp1, app1key2, app1key22])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key3])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key4])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key5])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key6])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key7])),
       ?assertEqual(app2value1, doteki:get_env([unsetapp2, app2key1])),
       ?assertEqual(undefined, doteki:get_env([unsetapp2, app2key2, app2key21])),
       ?assertEqual(123, doteki:get_env([unsetapp2, app2key2, app2key22]))
   end
   , fun() ->
       ok = doteki:unset_env([unsetapp1, app1key2, app1key21]),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key1])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key2, app1key21])),
       ?assertEqual(app1value22, doteki:get_env([unsetapp1, app1key2, app1key22])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key3])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key4])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key5])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key6])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key7])),
       ?assertEqual(app2value1, doteki:get_env([unsetapp2, app2key1])),
       ?assertEqual(undefined, doteki:get_env([unsetapp2, app2key2, app2key21])),
       ?assertEqual(123, doteki:get_env([unsetapp2, app2key2, app2key22]))
   end
   , fun() ->
       ok = doteki:unset_env([unsetapp1, app1key2]),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key1])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key2, app1key21])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key2, app1key22])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key2])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key3])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key4])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key5])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key6])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key7])),
       ?assertEqual(app2value1, doteki:get_env([unsetapp2, app2key1])),
       ?assertEqual(undefined, doteki:get_env([unsetapp2, app2key2, app2key21])),
       ?assertEqual(123, doteki:get_env([unsetapp2, app2key2, app2key22]))
   end
   , fun() ->
       ok = doteki:unset_env(unsetapp2),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key1])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key2, app1key21])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key2, app1key22])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key2])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key3])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key4])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key5])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key6])),
       ?assertEqual(undefined, doteki:get_env([unsetapp1, app1key7])),
       ?assertEqual(undefined, doteki:get_env([unsetapp2, app2key1])),
       ?assertEqual(undefined, doteki:get_env([unsetapp2, app2key2, app2key21])),
       ?assertEqual(undefined, doteki:get_env([unsetapp2, app2key2, app2key22]))
   end
  ].


function() -> 123.
function(A) -> A * 2.
function_error() -> missing:function().

