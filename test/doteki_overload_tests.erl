-module(doteki_overload_tests).

-include_lib("eunit/include/eunit.hrl").

-define(CONF, [
               {app, [
                      {key1, 'env.CUSTOM_ENV_1'},
                      {key2, {system, "CUSTOM_ENV_2"}},
                      {key3, {system, "CUSTOM_ENV_3", default}},
                      {key4, {system, "CUSTOM_ENV_4", as, binary}},
                      {key5, {system, "CUSTOM_ENV_5", as, binary, default}},
                      {key6, {system, "APP_KEY6", as, binary, default}}
                     ]}
              ]).


doteki_overload_test_() ->
  {setup,
   fun() ->
       [os:unsetenv(X) ||
        X <- ["CUSTOM_ENV_1", "CUSTOM_ENV_2", "CUSTOM_ENV_3", "CUSTOM_ENV_4", "CUSTOM_ENV_5",
              "APP_KEY1", "APP_KEY2", "APP_KEY3", "APP_KEY4", "APP_KEY5", "APP_KEY6"]]
   end,
   fun(_) ->
       [os:unsetenv(X) ||
        X <- ["CUSTOM_ENV_1", "CUSTOM_ENV_2", "CUSTOM_ENV_3", "CUSTOM_ENV_4", "CUSTOM_ENV_5",
              "APP_KEY1", "APP_KEY2", "APP_KEY3", "APP_KEY4", "APP_KEY5", "APP_KEY6"]]
   end,
   [
    fun() ->
        ?assertMatch(ok, doteki:set_env_from_config(?CONF))
    end
    , fun() ->
          ?assertEqual(undefined, doteki:get_env([app, key1])),
          ?assertEqual(undefined, doteki:get_env([app, key2])),
          ?assertEqual(default, doteki:get_env([app, key3])),
          ?assertEqual(undefined, doteki:get_env([app, key4])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key5])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key6]))
      end
    , fun() ->
          os:putenv("CUSTOM_ENV_1", "hello"),
          ?assertEqual(hello, doteki:get_env([app, key1])),
          ?assertEqual(undefined, doteki:get_env([app, key2])),
          ?assertEqual(default, doteki:get_env([app, key3])),
          ?assertEqual(undefined, doteki:get_env([app, key4])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key5])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key6]))
      end
    , fun() ->
          os:putenv("CUSTOM_ENV_2", "hello"),
          ?assertEqual(hello, doteki:get_env([app, key1])),
          ?assertEqual(hello, doteki:get_env([app, key2])),
          ?assertEqual(default, doteki:get_env([app, key3])),
          ?assertEqual(undefined, doteki:get_env([app, key4])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key5])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key6]))
      end
    , fun() ->
          os:putenv("CUSTOM_ENV_3", "hello"),
          ?assertEqual(hello, doteki:get_env([app, key1])),
          ?assertEqual(hello, doteki:get_env([app, key2])),
          ?assertEqual(hello, doteki:get_env([app, key3])),
          ?assertEqual(undefined, doteki:get_env([app, key4])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key5])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key6]))
      end
    , fun() ->
          os:putenv("CUSTOM_ENV_4", "hello"),
          ?assertEqual(hello, doteki:get_env([app, key1])),
          ?assertEqual(hello, doteki:get_env([app, key2])),
          ?assertEqual(hello, doteki:get_env([app, key3])),
          ?assertEqual(<<"hello">>, doteki:get_env([app, key4])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key5])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key6]))
      end
    , fun() ->
          os:putenv("CUSTOM_ENV_5", "hello"),
          ?assertEqual(hello, doteki:get_env([app, key1])),
          ?assertEqual(hello, doteki:get_env([app, key2])),
          ?assertEqual(hello, doteki:get_env([app, key3])),
          ?assertEqual(<<"hello">>, doteki:get_env([app, key4])),
          ?assertEqual(<<"hello">>, doteki:get_env([app, key5])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key6]))
      end
    , fun() ->
          os:putenv("APP_KEY1", "world"),
          ?assertEqual(world, doteki:get_env([app, key1])),
          ?assertEqual(hello, doteki:get_env([app, key2])),
          ?assertEqual(hello, doteki:get_env([app, key3])),
          ?assertEqual(<<"hello">>, doteki:get_env([app, key4])),
          ?assertEqual(<<"hello">>, doteki:get_env([app, key5])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key6]))
      end
    , fun() ->
          os:putenv("APP_KEY2", "world"),
          ?assertEqual(world, doteki:get_env([app, key1])),
          ?assertEqual(world, doteki:get_env([app, key2])),
          ?assertEqual(hello, doteki:get_env([app, key3])),
          ?assertEqual(<<"hello">>, doteki:get_env([app, key4])),
          ?assertEqual(<<"hello">>, doteki:get_env([app, key5])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key6]))
      end
    , fun() ->
          os:putenv("APP_KEY3", "world"),
          ?assertEqual(world, doteki:get_env([app, key1])),
          ?assertEqual(world, doteki:get_env([app, key2])),
          ?assertEqual(world, doteki:get_env([app, key3])),
          ?assertEqual(<<"hello">>, doteki:get_env([app, key4])),
          ?assertEqual(<<"hello">>, doteki:get_env([app, key5])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key6]))
      end
    , fun() ->
          os:putenv("APP_KEY4", "world"),
          ?assertEqual(world, doteki:get_env([app, key1])),
          ?assertEqual(world, doteki:get_env([app, key2])),
          ?assertEqual(world, doteki:get_env([app, key3])),
          ?assertEqual(world, doteki:get_env([app, key4])),
          ?assertEqual(<<"hello">>, doteki:get_env([app, key5])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key6]))
      end
    , fun() ->
          os:putenv("APP_KEY5", "world"),
          ?assertEqual(world, doteki:get_env([app, key1])),
          ?assertEqual(world, doteki:get_env([app, key2])),
          ?assertEqual(world, doteki:get_env([app, key3])),
          ?assertEqual(world, doteki:get_env([app, key4])),
          ?assertEqual(world, doteki:get_env([app, key5])),
          ?assertEqual(<<"default">>, doteki:get_env([app, key6]))
      end
    , fun() ->
          os:putenv("APP_KEY6", "world"),
          ?assertEqual(world, doteki:get_env([app, key6]))
      end
   ]}.

