

# Module doteki #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_env-1">get_env/1</a></td><td>Equivalent to <a href="#get_env-2"><tt>get_env(Path, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#get_env-2">get_env/2</a></td><td> 
Return the evironment value from the environment variable, or the configuration file, or 
the default value.</td></tr><tr><td valign="top"><a href="#get_env-3">get_env/3</a></td><td> 
Return the evironment value from the environment variable, or the configuration file, or 
the default value.</td></tr><tr><td valign="top"><a href="#set_env-2">set_env/2</a></td><td> 
Load a configuration for the given app with the given configuration.</td></tr><tr><td valign="top"><a href="#set_env_from_config-1">set_env_from_config/1</a></td><td> 
Load an application configuration from the given configuration.</td></tr><tr><td valign="top"><a href="#set_env_from_file-1">set_env_from_file/1</a></td><td> 
Load an application configuration from the given config file.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_env-1"></a>

### get_env/1 ###

<pre><code>
get_env(Path::[atom()]) -&gt; undefined | {ok, term()}
</code></pre>
<br />

Equivalent to [`get_env(Path, undefined)`](#get_env-2).

<a name="get_env-2"></a>

### get_env/2 ###

<pre><code>
get_env(App::atom() | [atom()], Key::atom() | [atom()] | term()) -&gt; undefined | {ok, term()}
</code></pre>
<br />


Return the evironment value from the environment variable, or the configuration file, or 
the default value.

Example:

If you call `doteki:get_env(app, key)`, id the `APP_KEY` environment
variable is set, _doteki_ will return it value. Else it will search for an existing
value for the `key` for the `app` in the configuration.

Calling `doteki:get_env([app, keyone, keytwo], "default")` return :

* the value  of the environment variable `APP_KEYONE_KEYTWO` of it exists

* else the value for `keytwo` in the dict returned by `keyone` for the `app`

* else `"default"`


<a name="get_env-3"></a>

### get_env/3 ###

<pre><code>
get_env(App::atom(), Key::atom() | [atom()], Default::term()) -&gt; undefined | {ok, term()}
</code></pre>
<br />


Return the evironment value from the environment variable, or the configuration file, or 
the default value.

Example:

If you call `doteki:get_env(app, key)`, id the `APP_KEY` environment
variable is set, _doteki_ will return it value. Else it will search for an existing
value for the `key` for the `app` in the configuration.

Calling `doteki:get_env([app, keyone, keytwo], "default")` return :

* the value  of the environment variable `APP_KEYONE_KEYTWO` of it exists

* else the value for `keytwo` in the dict returned by `keyone` for the `app`

* else `"default"`


<a name="set_env-2"></a>

### set_env/2 ###

<pre><code>
set_env(AppName::atom(), Config::[term()]) -&gt; ok | {error, any()}
</code></pre>
<br />


Load a configuration for the given app with the given configuration

Example :

```erlang

 undefined = application:get_env(test, key).
 doteki:set_env(test, [{key, "value"}]).
 {ok, Value} = application:get_env(test, key).
```

WARNING :

Call this function *after* loading your application. This is not mandatory but the
environment defined in the app file won't be loaded if an other env was loaded before.

<a name="set_env_from_config-1"></a>

### set_env_from_config/1 ###

<pre><code>
set_env_from_config(Rest::[term()]) -&gt; ok | {error, any()}
</code></pre>
<br />


Load an application configuration from the given configuration

Example :

```erlang

 undefined = doteki:get_env(test, key).
 doteki:set_env_from_file([{test, [{key, "value"}]}]).
 {ok, Value} = doteki:get_env(test, key).
```

WARNING :

Call this function *after* loading your application. This is not mandatory but the
environment defined in the app file won't be loaded if an other env was loaded before.

<a name="set_env_from_file-1"></a>

### set_env_from_file/1 ###

<pre><code>
set_env_from_file(File::<a href="file.md#type-filename">file:filename()</a>) -&gt; ok | {error, any()}
</code></pre>
<br />


Load an application configuration from the given config file

Example :

```erlang

 undefined = doteki:get_env(test, key).
 doteki:set_env_from_file("path/to/sys.config").
 {ok, Value} = doteki:get_env(test, key).
```

WARNING :

Call this function *after* loading your application. This is not mandatory but the
environment defined in the app file won't be loaded if an other env was loaded before.

