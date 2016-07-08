-module(doteki).

-export([
           get_all_env/0
         , get_all_env/1
         , get_env/1
         , get_env/2
         , get_env/3
         , get_as_string/1
         , get_as_string/2
         , get_as_string/3
         , get_as_atom/1
         , get_as_atom/2
         , get_as_atom/3
         , get_as_integer/1
         , get_as_integer/2
         , get_as_integer/3
         , get_as_float/1
         , get_as_float/2
         , get_as_float/3
         , get_as_binary/1
         , get_as_binary/2
         , get_as_binary/3
         , get_as_list/1
         , get_as_list/2
         , get_as_list/3
         , get_as_term/1
         , get_as_term/2
         , get_as_term/3
         , set_env/2
         , set_env/3
         , set_env_from_file/1
         , set_env_from_config/1
         , unset_env/1
         , compile_file/2
         , compile/0
         , compile/1
         , reload_env/0
         , main/1
        ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-export([test_fun/1]).
-endif.

% @hidden
main([In, Out|PA]) ->
  [code:add_patha(D) || D <- PA],
  case compile_file(In, Out) of
    ok ->
      erlang:halt(0);
    {error, Error} ->
      io:format("Compilation faild: ~p~n", [Error]),
      erlang:halt(1)
  end;
main(_) ->
  application:load(?MODULE),
  {ok, Vsn} = application:get_key(?MODULE, vsn),
  io:format("~s ~s~n~nUsage: ~s <in.config> <out.config> [<ebin paths>, ...]~n", [?MODULE, Vsn, ?MODULE]).

% @doc
% Sets the value of configuration parameter <tt>Key</tt> for <tt>App</tt>.
%
% Example :
%
% <pre lang="erlang">
% undefined = application:get_env(test, key).
% doteki:set_env(test, key, "value").
% {ok, Value} = application:get_env(test, key).
% </pre>
%
% WARNING :
%
% Call this function <b>after</b> loading your application. This is not mandatory but the
% environment defined in the app file won't be loaded if an other env was loaded before.
% @end
-spec set_env(atom(), atom(), term()) -> ok.
set_env(App, Key, Value) when is_atom(App), is_atom(Key) ->
  application:set_env(App, Key, Value).

% @doc
% Load a configuration for the given app with the given configuration
%
% Example :
%
% <pre lang="erlang">
% undefined = application:get_env(test, key).
% doteki:set_env(test, [{key, "value"}]).
% {ok, Value} = application:get_env(test, key).
% </pre>
%
% WARNING :
%
% Call this function <b>after</b> loading your application. This is not mandatory but the
% environment defined in the app file won't be loaded if an other env was loaded before.
% @end
-spec set_env(atom(), [term()]) -> ok | {error, any()}.
set_env(_, []) ->
  ok;
set_env(App, [{Key, Value}|Rest]) ->
  case set_env(App, Key, Value) of
    ok ->
      set_env(App, Rest);
    Error ->
      Error
  end.

% @doc
% Remove the configuration parameters and its value for the <tt>Application</tt> or the
% given <tt>Path</tt>
% @end
-spec unset_env([atom()] | atom()) -> ok.
unset_env(App) when is_atom(App) ->
  lists:foreach(fun({Par, _}) ->
                    unset_env([App, Par])
                end, application:get_all_env(App));
unset_env([App, Par]) ->
  application:unset_env(App, Par);
unset_env([App, Par|Keys]) ->
  case application:get_env(App, Par) of
    undefined ->
      ok;
    {ok, Val} ->
      Env = unset_env_key(Keys, Val),
      application:set_env(App, Par, Env)
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
% Call this function <b>after</b> loading your application. This is not mandatory but the
% environment defined in the app file won't be loaded if an other env was loaded before.
% @end
-spec set_env_from_config([term()]) -> ok | {error, any()}.
set_env_from_config([]) ->
  ok;
set_env_from_config([{App, Config}|Rest]) ->
  ok = unset_env(App),
  case set_env(App, Config) of
    ok ->
      set_env_from_config(Rest);
    Error ->
      Error
  end.

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
% Call this function <b>after</b> loading your application. This is not mandatory but the
% environment defined in the app file won't be loaded if an other env was loaded before.
% @end
set_env_from_file(File) ->
  case load_config_file(File) of
    {ok, Terms} ->
      set_env_from_config(Terms);
    {error, _, E} ->
      {error, E}
  end.

% @doc
% Returns the configuration parameters and their values for the application of the calling process.
% @end
-spec get_all_env() -> [term()].
get_all_env() ->
  case application:get_application() of
    undefined -> [];
    {ok, Cur} -> get_all_env(Cur)
  end.

% @doc
% Returns the configuration parameters and their values for Application.
% @end
-spec get_all_env(Application :: atom()) -> [term()].
get_all_env(Application) ->
  get_all_env(application:get_all_env(Application), [Application], []).

% @equiv get_env(Path, undefined)
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
%   <li>the value of the environment variable <tt>APP_KEYONE_KEYTWO</tt> of it exists</li>
%   <li>else the value for <tt>keytwo</tt> in the dict returned by <tt>keyone</tt> for the <tt>app</tt></li>
%   <li>else <tt>"default"</tt></li>
% </ul>
% @end
-spec get_env(atom() | [atom()] | [[atom()]], atom() | [atom()] | term()) -> undefined | term().
get_env(App, Key) when is_atom(App), is_atom(Key) ->
  get_env([App, Key]);
get_env(App, Path) when is_atom(App), is_list(Path) ->
  get_env([App|Path]);
get_env(Path, Default) when is_list(Path) ->
  case lists:all(fun erlang:is_atom/1, Path) of
    true ->
      get_one_env(Path, Default);
    false ->
      case lists:all(fun erlang:is_list/1, Path) of
        true ->
          get_first_env(Path, Default);
        false ->
          Default
      end
  end.

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
-spec get_env(atom(), atom() | [atom()], term()) -> undefined | term().
get_env(App, Key, Default) when is_atom(App), is_atom(Key) ->
  get_env([App, Key], Default);
get_env(App, Path, Default) when is_atom(App), is_list(Path) ->
  get_env([App|Path], Default).

% @doc
% Compile the configuration of the current application and all loaded applications.
% @end
compile() ->
  Apps = [App || {App, _, _} <- application:loaded_applications()],
  AllApps = case application:get_application() of
              undefined -> Apps;
              {ok, Cur} -> [Cur|Apps]
            end,
  compile(AllApps).

% @doc
% Compile the configuration of the given application/s
% @end
compile(Apps) when is_list(Apps) ->
  case lists:foldl(fun(App, Acc) ->
                       case compile(App) of
                         ok -> Acc;
                         {error, _} -> [App|Acc]
                       end
                   end, [], Apps) of
    [] -> ok;
    L -> {error, L}
  end;
compile(App) when is_atom(App) ->
  Terms = get_all_env(App),
  case set_env_from_config([{App, Terms}]) of
    ok -> ok;
    {error, _} -> {error, App}
  end.

% @doc
% Reload the configuration file passed to the VM (<tt>-config</tt> option)
% @end
-spec reload_env() -> ok | {error, any()}.
reload_env() ->
  case init:get_argument(config) of
    {ok, [[File]]} ->
      set_env_from_file(File);
    _ ->
      {error, missing_config}
  end.

% @doc
% Return the result of <tt>get_env/1</tt> as string
% @end
get_as_string(Path) ->
  case get_env(Path) of
    undefined -> undefined;
    Other -> bucs:to_string(Other)
  end.

% @doc
% Return the result of <tt>get_env/2</tt> as string
% @end
get_as_string(A, B) ->
  case get_env(A, B) of
    undefined -> undefined;
    Other -> bucs:to_string(Other)
  end.

% @doc
% Return the result of <tt>get_env/3</tt> as string
% @end
get_as_string(A, B, C) ->
  case get_env(A, B, C) of
    undefined -> undefined;
    Other -> bucs:to_string(Other)
  end.

% @doc
% Return the result of <tt>get_env/1</tt> as atom
% @end
get_as_atom(Path) ->
  case get_env(Path) of
    undefined -> undefined;
    Other -> bucs:to_atom(Other)
  end.

% @doc
% Return the result of <tt>get_env/2</tt> as atom
% @end
get_as_atom(A, B) ->
  case get_env(A, B) of
    undefined -> undefined;
    Other -> bucs:to_atom(Other)
  end.

% @doc
% Return the result of <tt>get_env/3</tt> as atom
% @end
get_as_atom(A, B, C) ->
  case get_env(A, B, C) of
    undefined -> undefined;
    Other -> bucs:to_atom(Other)
  end.

% @doc
% Return the result of <tt>get_env/1</tt> as integer
% @end
get_as_integer(Path) ->
  case get_env(Path) of
    undefined -> undefined;
    Other -> bucs:to_integer(Other)
  end.

% @doc
% Return the result of <tt>get_env/2</tt> as integer
% @end
get_as_integer(A, B) ->
  case get_env(A, B) of
    undefined -> undefined;
    Other -> bucs:to_integer(Other)
  end.

% @doc
% Return the result of <tt>get_env/3</tt> as integer
% @end
get_as_integer(A, B, C) ->
  case get_env(A, B, C) of
    undefined -> undefined;
    Other -> bucs:to_integer(Other)
  end.

% @doc
% Return the result of <tt>get_env/1</tt> as float
% @end
get_as_float(Path) ->
  case get_env(Path) of
    undefined -> undefined;
    Other -> bucs:to_float(Other)
  end.

% @doc
% Return the result of <tt>get_env/2</tt> as float
% @end
get_as_float(A, B) ->
  case get_env(A, B) of
    undefined -> undefined;
    Other -> bucs:to_float(Other)
  end.

% @doc
% Return the result of <tt>get_env/3</tt> as float
% @end
get_as_float(A, B, C) ->
  case get_env(A, B, C) of
    undefined -> undefined;
    Other -> bucs:to_float(Other)
  end.

% @doc
% Return the result of <tt>get_env/1</tt> as binary
% @end
get_as_binary(Path) ->
  case get_env(Path) of
    undefined -> undefined;
    Other -> bucs:to_binary(Other)
  end.

% @doc
% Return the result of <tt>get_env/2</tt> as binary
% @end
get_as_binary(A, B) ->
  case get_env(A, B) of
    undefined -> undefined;
    Other -> bucs:to_binary(Other)
  end.

% @doc
% Return the result of <tt>get_env/3</tt> as binary
% @end
get_as_binary(A, B, C) ->
  case get_env(A, B, C) of
    undefined -> undefined;
    Other -> bucs:to_binary(Other)
  end.

% @doc
% Return the result of <tt>get_env/1</tt> as list
% @end
get_as_list(Path) ->
  case get_env(Path) of
    undefined -> undefined;
    Other -> bucs:to_list(Other)
  end.

% @doc
% Return the result of <tt>get_env/2</tt> as list
% @end
get_as_list(A, B) ->
  case get_env(A, B) of
    undefined -> undefined;
    Other -> bucs:to_list(Other)
  end.

% @doc
% Return the result of <tt>get_env/3</tt> as list
% @end
get_as_list(A, B, C) ->
  case get_env(A, B, C) of
    undefined -> undefined;
    Other -> bucs:to_list(Other)
  end.

% @doc
% Return the result of <tt>get_env/1</tt> as term
% @end
get_as_term(Path) ->
  case get_env(Path) of
    undefined -> undefined;
    Other ->
      case bucs:to_term(Other) of
        {ok, Term} -> Term;
        _ -> undefined
      end
  end.

% @doc
% Return the result of <tt>get_env/2</tt> as term
% @end
get_as_term(A, B) ->
  case get_env(A, B) of
    undefined -> undefined;
    Other ->
      case bucs:to_term(Other) of
        {ok, Term} -> Term;
        _ -> undefined
      end
  end.

% @doc
% Return the result of <tt>get_env/3</tt> as term
% @end
get_as_term(A, B, C) ->
  case get_env(A, B, C) of
    undefined -> undefined;
    Other ->
      case bucs:to_term(Other) of
        {ok, Term} -> Term;
        _ -> undefined
      end
  end.

% @doc
% Compile a configuration file and save the result in an other file
% @end
-spec compile_file(file:filename(), file:filename()) -> ok | {error, term()}.
compile_file(In, Out) ->
  case load_config_file(In) of
    {ok, Apps} ->
      Compiled = [compile_app(App) || App <- Apps],
      io:format("=======> ~p~n", [Compiled]),
      file:write_file(Out, format([get_all_os_env(Compiled)]));
    {error, _, E} ->
      {error, E}
  end.

compile_app({App, Terms}) ->
  io:format("== compile app ~p~n", [App]),
  {App, [compile_terms(T) || T <- Terms]}.

compile_terms({Key, Value}) ->
  case compile_term(Value) of
    {ok, Result} ->
      {Key, Result};
    undefined ->
      {Key, undefined}
  end.


% Private

get_all_os_env(Config) ->
  get_all_os_env(Config, []).

get_all_os_env([], Acc) ->
  lists:reverse(Acc);
get_all_os_env([{App, Config}|Next], Acc) ->
  get_all_os_env(Next, [{App, get_all_os_env(Config, [App], [])}|Acc]).

get_all_os_env([], _, Result) ->
  lists:reverse(Result);
get_all_os_env([{Key, Value}|Rest], Path, Result) ->
  case os_get_env(lists:reverse([Key|Path])) of
    false ->
      case is_list(Value) andalso not bucs:is_string(Value) of
        true ->
          get_all_os_env(Rest, Path, [{Key, get_all_os_env(Value, [Key|Path], [])}|Result]);
        false ->
          get_all_os_env(Rest, Path, [{Key, Value}|Result])
      end;
    EnvVal ->
      case env_var_to_val(EnvVal) of
        {ok, EnvVal} ->
          get_all_os_env(Rest, Path, [{Key, undefined}|Result]);
        {ok, Other} ->
          get_all_os_env(Rest, Path, [{Key, Other}|Result])
      end
  end;
get_all_os_env(Value, _, _) ->
  Value.

get_all_env([], _, Result) ->
  lists:reverse(Result);
get_all_env([{Key, Value}|Rest], Path, Result) ->
  case is_list(Value) and not bucs:is_string(Value) of
    true ->
      get_all_env(Rest, Path, [{Key, get_all_env(Value, [Key|Path], [])}|Result]);
    false ->
      get_all_env(Rest, Path, [{Key, get_env(lists:reverse([Key|Path]))}|Result])
  end.

get_first_env([], Default) ->
  Default;
get_first_env([Path|Rest], Default) ->
  case get_one_env(Path, undefined) of
    undefined ->
      get_first_env(Rest, Default);
    Other ->
      Other
  end.
get_one_env([App|Keys] = Path, Default) ->
  case os_get_env(Path) of
    false ->
      Env = application:get_all_env(App),
      Result = find_env(Keys, Env, Default),
      case compile_term(Result) of
        {ok, CompiledResult} ->
          CompiledResult;
        undefined ->
          Default
      end;
    EnvVal ->
      case env_var_to_val(EnvVal) of
        {ok, EnvVal} ->
          Default;
        {ok, Other} ->
          Other
      end
  end.

os_get_env(Path) ->
  EnvVar = string:join(
             [string:to_upper(E1)
              || E1 <- [bucs:to_string(E0)
                        || E0 <- Path]],
             "_"),
  os:getenv(EnvVar).

find_env([], Env, _) ->
  Env;
find_env([Key|Rest], Env, Default) when is_list(Env) ->
  case lists:keyfind(Key, 1, Env) of
    false ->
      Default;
    {Key, SubEnv} ->
      find_env(Rest, SubEnv, Default)
  end;
find_env(_, _, Default) ->
  Default.

unset_env_key([Key], Val) ->
  lists:keydelete(Key, 1, Val);
unset_env_key([Key|Keys], Val) ->
  case lists:keyfind(Key, 1, Val) of
    {Key, Sub} ->
      Env = unset_env_key(Keys, Sub),
      lists:keyreplace(Key, 1, Val, Env);
    _ ->
      Val
  end.

load_config_file(File) ->
  case file:consult(File) of
    {ok, [Data]} -> check_config(Data);
    {error, Error} -> {error, File, Error}
  end.

check_config(Data) ->
  check_config(Data, []).
check_config([], Acc) ->
  {ok, merge_config(Acc)};
check_config([File|Rest], Acc) when is_list(File) ->
  case load_config_file(File) of
    {ok, Data} ->
      check_config(Rest, lists:reverse(Data) ++ Acc);
    Error ->
      Error
  end;
check_config([Term|Rest], Acc) ->
  check_config(Rest, [Term|Acc]).

merge_config(Data) ->
  lists:foldl(fun({App, Values} = T, Acc) ->
                  case lists:keyfind(App, 1, Acc) of
                    {App, ExistingValues} ->
                      lists:keyreplace(App, 1, Acc, {App, merge_values(Values, ExistingValues)});
                    false ->
                      [T|Acc]
                  end
              end, [], Data).

merge_values([], Acc) ->
  Acc;
merge_values([Value|Rest], Acc) when is_tuple(Value) ->
  case lists:keyfind(erlang:element(1, Value), 1, Acc) of
    false ->
      merge_values(Rest, [Value|Acc]);
    _ ->
      merge_values(Rest, Acc)
  end;
merge_values([Value|Rest], Acc) ->
  case lists:member(Value, Acc) of
    true ->
      merge_values(Rest, Acc);
    false ->
      merge_values(Rest, [Value|Acc])
  end.

% {env, "ENV_VAR"}
compile_term({env, ENV}) ->
  compile_term({system, ENV, undefined});
% {system, "ENV_VAR"}
compile_term({system, ENV}) ->
  compile_term({system, ENV, undefined});
% {env, "ENV_VAR", Default}
compile_term({env, ENV, Default}) ->
  compile_term({system, ENV, Default});
% {system, "ENV_VAR", Default}
compile_term({system, ENV, Default}) ->
  get_env_var(ENV, Default);
% {fun, Module, Function}
compile_term({'fun', Module, Function}) when is_atom(Module),
                                             is_atom(Function) ->
  compile_term({fn, Module, Function, 0, []});
% {fun, Module, Function, 0}
compile_term({'fun', Module, Function, 0}) when is_atom(Module),
                                                is_atom(Function) ->
  compile_term({fn, Module, Function, 0, []});
% {fun, module, function, Args}
compile_term({'fun', Module, Function, Args}) when is_atom(Module),
                                                   is_atom(Function),
                                                   is_list(Args) ->
  compile_term({fn, Module, Function, length(Args), Args});
% {fun, module, function, arity, Args}
compile_term({'fun', Module, Function, Arity, Args}) when is_atom(Module),
                                                          is_atom(Function),
                                                          is_integer(Arity),
                                                          is_list(Args) ->
  compile_term({fn, Module, Function, Arity, Args});
% {nf, Module, Function}
compile_term({fn, Module, Function}) when is_atom(Module),
                                          is_atom(Function) ->
  compile_term({fn, Module, Function, 0, []});
% {nf, Module, Function, 0}
compile_term({fn, Module, Function, 0}) when is_atom(Module),
                                             is_atom(Function) ->
  compile_term({fn, Module, Function, 0, []});
% {fn, module, function, Args}
compile_term({fn, Module, Function, Args}) when is_atom(Module),
                                                is_atom(Function),
                                                is_list(Args) ->
  compile_term({fn, Module, Function, length(Args), Args});
% {fn, module, function, arity, Args}
compile_term({fn, Module, Function, Arity, Args}) when is_atom(Module),
                                                       is_atom(Function),
                                                       is_integer(Arity),
                                                       is_list(Args) ->
  case compile_term(Args) of
    {ok, CompiledArgs} ->
      case length(CompiledArgs) of
        Arity ->
          case bucs:function_exists(Module, Function, Arity) of
            true ->
              try
                {ok, erlang:apply(Module, Function, CompiledArgs)}
              catch
                _:_ ->
                  undefined
              end;
            false ->
              undefined
          end;
        _ ->
          undefined
      end;
    _ ->
      undefined
  end;
% {'fun.module:function/arity', Args}
% {'fun.module:function', Args}
% {'env.ENV_VAR', Default}
% Tuple
compile_term({Fun, Args} = Tuple) when is_atom(Fun) ->
  CompiledArgs = case compile_term(Args) of
                   {ok, R} -> R;
                   undefined -> undefined
                 end,
  case bucs:to_string(Fun) of
    "fun." ++ Fn ->
      case re:run(Fn, "^([^:]*):([^\/]*)(/(.*))$", [{capture, all_but_first, list}]) of
        {match, [Module, Function]} ->
          case CompiledArgs of
            CompiledArgs when is_list(CompiledArgs) ->
              compile_term({fn,
                            bucs:to_atom(Module),
                            bucs:to_atom(Function),
                            length(CompiledArgs),
                            CompiledArgs});
            _ ->
              undefined
          end;
        {match, [Module, Function, _, "0"]} ->
          {ok, {case compile_term({fn, bucs:to_atom(Module), bucs:to_atom(Function), 0, []}) of
                  {ok, R0} -> R0;
                  _ -> undefined
                end,
                case compile_term(Args) of
                  {ok, R1} -> R1;
                  _ -> undefined
                end}};
        {match, [Module, Function, _, Arity]} ->
          case bucs:to_integer(Arity) == length(CompiledArgs) of
            true ->
              compile_term({fn,
                            bucs:to_atom(Module),
                            bucs:to_atom(Function),
                            bucs:to_integer(Arity),
                            CompiledArgs});
            false ->
              undefined
          end;
        _ ->
          compile_tuple(Tuple)
      end;
    "env." ++ ENV ->
      get_env_var(ENV, CompiledArgs);
    _ ->
      compile_tuple(Tuple)
  end;
% Tuple
compile_term(Term) when is_tuple(Term) ->
  compile_tuple(Term);
% 'env.ENV_VAR'
% 'fun.module:function/0'
% 'fun.module:function'
% Atom
compile_term(Term) when is_atom(Term) ->
  case bucs:to_string(Term) of
    "fun." ++ Fun ->
      case re:run(Fun, "^([^:]*):([^\/]*)(/(.*))$", [{capture, all_but_first, list}]) of
        {match, [Module, Function]} ->
          compile_term({fn, bucs:to_atom(Module), bucs:to_atom(Function), 0, []});
        {match, [Module, Function, _, "0"]} ->
          compile_term({fn, bucs:to_atom(Module), bucs:to_atom(Function), 0, []});
        _ ->
          {ok, Term}
      end;
    "env." ++ ENV ->
      get_env_var(ENV, undefined);
    _ ->
      {ok, Term}
  end;
compile_term(Term) when is_list(Term) ->
  case bucs:is_string(Term) of
    true ->
      {ok, Term};
    false ->
      {ok, lists:reverse(
             lists:foldl(fun(T, Acc) ->
                             case compile_term(T) of
                               {ok, R} -> [R|Acc];
                               undefined -> Acc
                             end
                         end, [], Term))}
  end;
compile_term(Term) when is_map(Term) ->
  {ok, maps:map(fun(_, V) ->
                    case compile_term(V) of
                      {ok, R} -> R;
                      undefined -> undefined
                    end
                end, Term)};
compile_term(Term) ->
  {ok, Term}.

compile_tuple(Tuple) ->
  case compile_term(bucs:to_list(Tuple)) of
    {ok, Value} ->
      {ok, list_to_tuple(Value)};
    Other ->
      Other
  end.

get_env_var(ENV, Default) ->
  case os:getenv(bucs:to_string(ENV)) of
    false ->
      compile_term(Default);
    Value ->
      env_var_to_val(Value)
  end.

env_var_to_val(Value) ->
  case re:run(Value, "(.*):(atom|string|binary|integer|float|term)$", [{capture, all, list}]) of
    {match,[_, Value1, Type]} ->
      to_value(Value1, Type);
    _ ->
      to_value(Value, "term")
  end.

to_value(V, "string") -> {ok, V};
to_value(V, "atom") -> {ok, bucs:to_atom(V)};
to_value(V, "binary") -> {ok, bucs:to_binary(V)};
to_value(V, "integer") -> {ok, bucs:to_integer(V)};
to_value(V, "float") -> {ok, bucs:to_float(V)};
to_value(V, "term") ->
  try
    case bucs:to_term(V) of
      {ok, T} -> {ok, T};
      _ -> {ok, V}
    end
  catch
    _:_ ->
      {ok, V}
  end.

format(Terms) ->
  lists:foldl(fun(Term, Content) ->
                  Content ++ pretty(Term)
              end, "", Terms).

pretty(Term) ->
  Abstract = erl_syntax:abstract(Term),
  AnnF = fun(Node) -> annotate_tuple(Node) end,
  AnnAbstract = postorder(AnnF, Abstract),
  HookF = fun(Node, Ctxt, Cont) ->
              Doc = Cont(Node, Ctxt),
              prettypr:above(prettypr:empty(), Doc)
          end,
  io_lib:format("~s~n", [
                         lists:flatten(
                           erl_prettypr:format(
                             AnnAbstract, [{hook, HookF}, {paper, 160}, {ribbon, 145}])) ++ "."]).

annotate_tuple(Node) ->
  case erl_syntax:type(Node) of
    tuple -> erl_syntax:add_ann(tuple, Node);
    _ -> Node
  end.

postorder(F, Tree) ->
  F(case erl_syntax:subtrees(Tree) of
      [] -> Tree;
      List -> erl_syntax:update_tree(Tree,
                                     [[postorder(F, Subtree)
                                       || Subtree <- Group]
                                      || Group <- List])
    end).

-ifdef(TEST).
test_fun(X) ->
  X.
compile_term_test() ->
  ?assertEqual({ok, atom}, compile_term(atom)),
  ?assertEqual({ok, "string"}, compile_term("string")),
  ?assertEqual({ok, [1,2,3,4]}, compile_term([1,2,3,4])),
  ?assertEqual({ok, {a,b,c,d}}, compile_term({a,b,c,d})),
  ?assertEqual({ok, {a, "string", 1,
                     {a, "string", 1,
                      [1, a, "string",
                       {a, b}]},
                     [1, a, "string", {a, b}]}},
               compile_term({a, "string", 1,
                             {a, "string", 1,
                              [1, a, "string",
                               {a, b}]},
                             [1, a, "string", {a, b}]})),
  ?assertEqual({ok, "bonjour"}, compile_term({'env.DOTEKI_COMPILE_TERM_TEST_EN',
                                              {'env.DOTEKI_COMPILE_TERM_TEST_ES',
                                               "bonjour"}})),
  ?assertEqual({ok, "bonjour"}, compile_term({system, "DOTEKI_COMPILE_TERM_TEST_EN",
                                              {system, "DOTEKI_COMPILE_TERM_TEST_ES",
                                               "bonjour"}})),
  os:putenv("DOTEKI_COMPILE_TERM_TEST_ES", "\"hola\""),
  ?assertEqual({ok, "hola"}, compile_term({'env.DOTEKI_COMPILE_TERM_TEST_EN',
                                           {'env.DOTEKI_COMPILE_TERM_TEST_ES',
                                            "bonjour"}})),
  ?assertEqual({ok, "hola"}, compile_term({system, "DOTEKI_COMPILE_TERM_TEST_EN",
                                           {system, "DOTEKI_COMPILE_TERM_TEST_ES",
                                            "bonjour"}})),
  os:putenv("DOTEKI_COMPILE_TERM_TEST_EN", "\"hello\""),
  ?assertEqual({ok, "hello"}, compile_term({'env.DOTEKI_COMPILE_TERM_TEST_EN',
                                            {'env.DOTEKI_COMPILE_TERM_TEST_ES',
                                             "bonjour"}})),
  ?assertEqual({ok, "hello"}, compile_term({system, "DOTEKI_COMPILE_TERM_TEST_EN",
                                            {system, "DOTEKI_COMPILE_TERM_TEST_ES",
                                             "bonjour"}})),
  ?assertEqual({ok, {"hello", "world"}}, compile_term({'fun.doteki:test_fun/1',
                                                       [{{system, "DOTEKI_COMPILE_TERM_TEST_EN"}, "world"}]})),
  ?assertEqual({ok, {"hello", "world"}}, compile_term({'fun', ?MODULE, test_fun, 1,
                                                       [{{'env.DOTEKI_COMPILE_TERM_TEST_EN', undefined}, "world"}]})),
  ?assertEqual(undefined, compile_term({'fun', ?MODULE, test_fun})).
-endif.
