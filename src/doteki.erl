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
         , set_env_from_file/1
         , set_env_from_config/1
         , unset_env/1
         , compile_file/2
         , compile/0
         , compile/1
         , reload_env/0
        ]).

-define(UNDEFINED, undefined).

% @doc
% Return the result of <tt>get_env/1</tt> as string
% @end
get_as_string(Path) ->
  case get_env(Path) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_string(Other)
  end.

% @doc
% Return the result of <tt>get_env/2</tt> as string
% @end
get_as_string(A, B) ->
  case get_env(A, B) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_string(Other)
  end.

% @doc
% Return the result of <tt>get_env/3</tt> as string
% @end
get_as_string(A, B, C) ->
  case get_env(A, B, C) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_string(Other)
  end.

% @doc
% Return the result of <tt>get_env/1</tt> as atom
% @end
get_as_atom(Path) ->
  case get_env(Path) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_atom(Other)
  end.

% @doc
% Return the result of <tt>get_env/2</tt> as atom
% @end
get_as_atom(A, B) ->
  case get_env(A, B) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_atom(Other)
  end.

% @doc
% Return the result of <tt>get_env/3</tt> as atom
% @end
get_as_atom(A, B, C) ->
  case get_env(A, B, C) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_atom(Other)
  end.

% @doc
% Return the result of <tt>get_env/1</tt> as integer
% @end
get_as_integer(Path) ->
  case get_env(Path) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_integer(Other)
  end.

% @doc
% Return the result of <tt>get_env/2</tt> as integer
% @end
get_as_integer(A, B) ->
  case get_env(A, B) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_integer(Other)
  end.

% @doc
% Return the result of <tt>get_env/3</tt> as integer
% @end
get_as_integer(A, B, C) ->
  case get_env(A, B, C) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_integer(Other)
  end.

% @doc
% Return the result of <tt>get_env/1</tt> as float
% @end
get_as_float(Path) ->
  case get_env(Path) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_float(Other)
  end.

% @doc
% Return the result of <tt>get_env/2</tt> as float
% @end
get_as_float(A, B) ->
  case get_env(A, B) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_float(Other)
  end.

% @doc
% Return the result of <tt>get_env/3</tt> as float
% @end
get_as_float(A, B, C) ->
  case get_env(A, B, C) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_float(Other)
  end.

% @doc
% Return the result of <tt>get_env/1</tt> as binary
% @end
get_as_binary(Path) ->
  case get_env(Path) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_binary(Other)
  end.

% @doc
% Return the result of <tt>get_env/2</tt> as binary
% @end
get_as_binary(A, B) ->
  case get_env(A, B) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_binary(Other)
  end.

% @doc
% Return the result of <tt>get_env/3</tt> as binary
% @end
get_as_binary(A, B, C) ->
  case get_env(A, B, C) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_binary(Other)
  end.

% @doc
% Return the result of <tt>get_env/1</tt> as list
% @end
get_as_list(Path) ->
  case get_env(Path) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_list(Other)
  end.

% @doc
% Return the result of <tt>get_env/2</tt> as list
% @end
get_as_list(A, B) ->
  case get_env(A, B) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_list(Other)
  end.

% @doc
% Return the result of <tt>get_env/3</tt> as list
% @end
get_as_list(A, B, C) ->
  case get_env(A, B, C) of
    ?UNDEFINED -> ?UNDEFINED;
    Other -> bucs:to_list(Other)
  end.

% @doc
% Return the result of <tt>get_env/1</tt> as term
% @end
get_as_term(Path) ->
  case get_env(Path) of
    ?UNDEFINED -> ?UNDEFINED;
    Other ->
      case bucs:to_term(Other) of
        {ok, Term} -> Term;
        _ -> ?UNDEFINED
      end
  end.

% @doc
% Return the result of <tt>get_env/2</tt> as term
% @end
get_as_term(A, B) ->
  case get_env(A, B) of
    ?UNDEFINED -> ?UNDEFINED;
    Other ->
      case bucs:to_term(Other) of
        {ok, Term} -> Term;
        _ -> ?UNDEFINED
      end
  end.

% @doc
% Return the result of <tt>get_env/3</tt> as term
% @end
get_as_term(A, B, C) ->
  case get_env(A, B, C) of
    ?UNDEFINED -> ?UNDEFINED;
    Other ->
      case bucs:to_term(Other) of
        {ok, Term} -> Term;
        _ -> ?UNDEFINED
      end
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

get_all_env([], _, Result) ->
  lists:reverse(Result);
get_all_env([{Key, Value}|Rest], Path, Result) ->
  case is_list(Value) and not bucs:is_string(Value) of
    true ->
      get_all_env(Rest, Path, [{Key, get_all_env(Value, [Key|Path], [])}|Result]);
    false ->
      get_all_env(Rest, Path, [{Key, get_env(lists:reverse([Key|Path]))}|Result])
  end.


% @equiv get_env(Path, undefined)
-spec get_env([atom()]) -> undefined | term().
get_env(Path) when is_list(Path) ->
  get_env(Path, ?UNDEFINED).

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
      bucs:pipecall([
                     {fun buclists:pipemap/2,
                      [[fun atom_to_list/1, fun string:to_upper/1], Path]},
                     {fun string:join/2, ["_"]},
                     {fun os_get_env/2, [get_env1(Path, Default)]},
                     {fun get_env3/2, [Default]},
                     fun compile_to_term/1
                    ]);
    false ->
      case lists:all(fun erlang:is_list/1, Path) of
        true ->
          get_env4(Path, Default);
        false ->
          undefined
      end
  end.

get_env1([App|Fields], Default) ->
  case get_env2(Fields, application:get_all_env(App)) of
    ?UNDEFINED ->
      Default;
    X ->
      X
  end.

get_env2(_, ?UNDEFINED) ->
  ?UNDEFINED;
get_env2([], Result) ->
  Result;
get_env2([Field|Rest], Data) ->
  get_env2(Rest, buclists:keyfind(Field, 1, Data, ?UNDEFINED)).

get_env3(Value, Default) when is_atom(Value) ->
  case bucs:to_string(Value) of
    "env." ++ EnvVar ->
      os_get_env(EnvVar, Default);
    "fun." ++ Fun ->
      case re:run(Fun, "^([^:]*):([^\/]*)/(.*)$", [{capture, all_but_first, list}]) of
        nomatch ->
          Default;
        {match,[Module, Function, "0"]} ->
          case bucs:apply(bucs:to_atom(Module),
                          bucs:to_atom(Function),
                          []) of
            error -> Default;
            {ok, Result} -> Result
          end
      end;
    _ ->
      Value
  end;
get_env3({Fun, Args} = Value, Default) when is_atom(Fun),
                                            is_list(Args) ->
  case bucs:to_string(Fun) of
    "fun." ++ Fun1 ->
      case re:run(Fun1, "^([^:]*):([^\/]*)/(.*)$", [{capture, all_but_first, list}]) of
        nomatch ->
          Default;
        {match,[Module, Function, N]} ->
          case list_to_integer(N) == length(Args) of
            true ->
              case bucs:apply(bucs:to_atom(Module),
                              bucs:to_atom(Function),
                              Args) of
                error -> Default;
                {ok, Result} -> Result
              end;
            false ->
              Default
          end
      end;
    _ ->
      Value
  end;
get_env3(Value, _) ->
  Value.

get_env4(Path, Default) ->
  get_env4(Path, Default, Default).

get_env4([Path|Rest], Default, Default) ->
  get_env4(Rest, get_env(Path, Default), Default);
get_env4(_, Result, _) ->
  Result.

os_get_env(Var, Default) ->
  case os:getenv(Var, Default) of
    Value when is_list(Value), Value =/= Default ->
      case bucs:is_string(Value) of
        true ->
          case re:run(Value, "(.*):(atom|string|binary|integer|float|term)$", [{capture, all, list}]) of
            {match,[_, Value1, Type]} ->
              to_value(Value1, bucs:to_atom(Type));
            _ ->
              to_value(Value, term)
          end;
        _ ->
          Value
      end;
    Other -> Other
  end.

to_value(V, string) -> V;
to_value(V, atom) -> bucs:to_atom(V);
to_value(V, binary) -> bucs:to_binary(V);
to_value(V, integer) -> bucs:to_integer(V);
to_value(V, float) -> bucs:to_float(V);
to_value(V, term) ->
  case bucs:to_term(V) of
    {ok, T} -> T;
    _ -> V
  end.
%  V1 = case lists:reverse(V) of
%         [$.|_] -> V;
%         _ -> V ++ "."
%       end,
%  {ok, Tokens, _} = erl_scan:string(V1),
%  case erl_parse:parse_term(Tokens) of
%    {ok, Term} -> Term;
%    _ -> ?UNDEFINED
%  end.

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
  case load_config_file(File) of
    {ok, Terms} ->
      set_env_from_config(Terms);
    {error, _, E} ->
      {error, E}
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
  ok = unset_env(AppName),
  case set_env(AppName, AppConfig) of
    ok -> set_env_from_config(Rest);
    E -> E
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
    ?UNDEFINED ->
      ok;
    {ok, Val} ->
      Env = unset_env_key(Keys, Val),
      application:set_env(App, Par, Env)
  end.

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

% @doc
% Compile a configuration file and save the result in an other file
% @end
-spec compile_file(file:filename(), file:filename()) -> ok | {error, term()}.
compile_file(In, Out) ->
  case file:consult(In) of
    {ok, [Terms]} ->
      Terms1 = case is_list(Terms) and not bucs:is_string(Terms) of
                 true -> Terms;
                 _ -> [Terms]
               end,
      bucs:pipecall([
                     {fun compile_to_term/1, [Terms1]},
                     fun format/1,
                     {fun write_file/2, [Out]}
                    ]);
    E -> E
  end.

% @doc
% Compile the configuration of the current application and all loaded applications.
% @end
-spec compile() -> ok | {error, [atom()]}.
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
-spec compile(atom() | [atom()]) -> ok | {error, atom()} | {error, [atom()]}.
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
      set_env_from_config(File);
    _ ->
      {error, missing_config}
  end.

% -- private functions --

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

compile_to_term(Term) ->
  case is_list(Term) andalso not bucs:is_string(Term) of
    true ->
      compile_to_term(Term, []);
    _ ->
      get_env3(Term, ?UNDEFINED)
  end.

compile_to_term([], Result) ->
  lists:reverse(Result);
compile_to_term([{Key, Term}|Terms], Result) ->
  compile_to_term(Terms, [{Key, compile_to_term(Term)}|Result]);
compile_to_term([X|Terms], Result) ->
  compile_to_term(Terms, [X|Result]).

write_file(Data, File) ->
  file:write_file(File, Data).

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

