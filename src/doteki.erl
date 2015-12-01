-module(doteki).

-export([
         get_env/1
         , get_env/2
         , get_env/3
         , set_env/2
         , set_env_from_file/1
         , set_env_from_config/1
        ]).

% @equiv get_env(Path, undefined)
-spec get_env([atom()]) -> undefined | {ok, term()}.
get_env(Path) when is_list(Path) -> 
  get_env(Path, undefined).

% @doc
% Return the evironment value from the environment variable, or the configuration file, or 
% the default value.
%
% Example:
%
% If you call <tt>doteki:get_env(app, key)</tt>, id the <tt>APP_KEY</tt> environment
% variable is set, <i>doteki</i> will return it value. Else it will search for an existing 
% value for the <tt>key</tt> for the <tt>app</tt> in the configuration.
%
% Calling <tt>doteki:get_env([app, keyone, keytwo], "default")</tt> return :
% <ul>
%   <li>the value  of the environment variable <tt>APP_KEYONE_KEYTWO</tt> of it exists</li>
%   <li>else the value for <tt>keytwo</tt> in the dict returned by <tt>keyone</tt> for the <tt>app</tt></li>
%   <li>else <tt>"default"</tt></li>
% </ul>
% @end
-spec get_env(atom() | [atom()], atom() | [atom()] | term()) -> undefined | {ok, term()}.
get_env(App, Key) when is_atom(App), is_atom(Key) ->
  get_env([App, Key]);
get_env(App, Path) when is_atom(App), is_list(Path) ->
  get_env([App|Path]);
get_env(Path, Default) when is_list(Path) ->
  bucs:pipecall([
                 {fun buclists:pipemap/2, 
                  [[fun atom_to_list/1, fun string:to_upper/1], Path]},
                 {fun string:join/2, ["_"]},
                 {fun os:getenv/2, [get_env1(Path, Default)]},
                 {fun get_env3/2, [Default]}
                ]).

get_env1([App|Fields], Default) ->
  case get_env2(Fields, application:get_all_env(App)) of
    undefined ->
      Default;
    X ->
      X
  end.

get_env2(_, undefined) ->
  undefined;
get_env2([], Result) ->
  Result;
get_env2([Field|Rest], Data) ->
  get_env2(Rest, buclists:keyfind(Field, 1, Data, undefined)).

get_env3(Value, Default) when is_atom(Value) ->
  case bucs:to_string(Value) of
    "env." ++ EnvVar ->
      os:getenv(EnvVar, Default);
    _ -> 
      Value
  end;
get_env3(Value, _) -> 
  Value.

% @doc
% Return the evironment value from the environment variable, or the configuration file, or 
% the default value.
%
% Example:
%
% If you call <tt>doteki:get_env(app, key)</tt>, id the <tt>APP_KEY</tt> environment
% variable is set, <i>doteki</i> will return it value. Else it will search for an existing 
% value for the <tt>key</tt> for the <tt>app</tt> in the configuration.
%
% Calling <tt>doteki:get_env([app, keyone, keytwo], "default")</tt> return :
% <ul>
%   <li>the value  of the environment variable <tt>APP_KEYONE_KEYTWO</tt> of it exists</li>
%   <li>else the value for <tt>keytwo</tt> in the dict returned by <tt>keyone</tt> for the <tt>app</tt></li>
%   <li>else <tt>"default"</tt></li>
% </ul>
% @end
-spec get_env(atom(), atom() | [atom()], term()) -> undefined | {ok, term()}.
get_env(App, Key, Default) when is_atom(App), is_atom(Key) ->
  get_env([App, Key], Default);
get_env(App, Path, Default) when is_atom(App), is_list(Path) ->
  get_env([App|Path], Default).

% @doc
% Load a configuration for the given app with the given configuration
%
% Example :
%
% <pre lang="erlang">
% undefined = application:get_env(test, key).
% eapplication:set_env(test, [{key, "value"}]).
% {ok, Value} = application:get_env(test, key).
% </pre>
%
% WARNING :
%
% Call this function *after* loading your application. This is not mandatory but the
% environment defined in the app file won't be loaded if an other env was loaded before.
% @end
-spec set_env(atom(), [term()]) -> ok | {error, any()}.
set_env(_, []) -> ok;
set_env(AppName, [{Key, Value}|Config]) ->
  _ = application:set_env(AppName, Key, Value),
  set_env(AppName, Config).

% @doc
% Load an application configuration from the given config file
%
% Example :
%
% <pre lang="erlang">
% undefined = doteki:get_env(test, key).
% doteki:set_env_from_file("path/to/sys.config").
% {ok, Value} = doteki:get_env(test, key).
% </pre>
%
% WARNING :
%
% Call this function *after* loading your application. This is not mandatory but the
% environment defined in the app file won't be loaded if an other env was loaded before.
% @end
-spec set_env_from_file(file:filename()) -> ok | {error, any()}.
set_env_from_file(File) ->
  case file:consult(File) of
    {ok, [Terms]} ->
      set_env_from_config(Terms);
    E ->
      E   
  end.

% @doc
% Load an application configuration from the given configuration
%
% Example :
%
% <pre lang="erlang">
% undefined = doteki:get_env(test, key).
% doteki:set_env_from_file([{test, [{key, "value"}]}]).
% {ok, Value} = doteki:get_env(test, key).
% </pre>
%
% WARNING :
%
% Call this function *after* loading your application. This is not mandatory but the
% environment defined in the app file won't be loaded if an other env was loaded before.
% @end
-spec set_env_from_config([term()]) -> ok | {error, any()}.
set_env_from_config([]) -> ok; 
set_env_from_config([{AppName, AppConfig}|Rest]) ->
  case set_env(AppName, AppConfig) of
    ok -> set_env_from_config(Rest);
    E -> E
  end.

