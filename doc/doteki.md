

# Module doteki #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compile-0">compile/0</a></td><td>
Compile the configuration of the current application and all loaded applications.</td></tr><tr><td valign="top"><a href="#compile-1">compile/1</a></td><td>
Compile the configuration of the given application/s.</td></tr><tr><td valign="top"><a href="#compile_file-2">compile_file/2</a></td><td>
Compile a configuration file and save the result in an other file.</td></tr><tr><td valign="top"><a href="#get_all_env-0">get_all_env/0</a></td><td>
Returns the configuration parameters and their values for the application of the calling process.</td></tr><tr><td valign="top"><a href="#get_all_env-1">get_all_env/1</a></td><td>
Returns the configuration parameters and their values for Application.</td></tr><tr><td valign="top"><a href="#get_env-1">get_env/1</a></td><td>Equivalent to <a href="#get_env-2"><tt>get_env(Path, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#get_env-2">get_env/2</a></td><td> 
Return the evironment value from the environment variable, or the configuration file, or 
the default value.</td></tr><tr><td valign="top"><a href="#get_env-3">get_env/3</a></td><td> 
Return the evironment value from the environment variable, or the configuration file, or 
the default value.</td></tr><tr><td valign="top"><a href="#set_env-2">set_env/2</a></td><td> 
Load a configuration for the given app with the given configuration.</td></tr><tr><td valign="top"><a href="#set_env_from_config-1">set_env_from_config/1</a></td><td> 
Load an application configuration from the given configuration.</td></tr><tr><td valign="top"><a href="#set_env_from_file-1">set_env_from_file/1</a></td><td> 
Load an application configuration from the given config file.</td></tr><tr><td valign="top"><a href="#unset_env-1">unset_env/1</a></td><td>
Remove the configuration parameters and its value for the <tt>Application</tt> or the
given <tt>Path</tt></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="compile-0"></a>

### compile/0 ###

<pre><code>
compile() -&gt; ok | {error, [atom()]}
</code></pre>
<br />

Compile the configuration of the current application and all loaded applications.

<a name="compile-1"></a>

### compile/1 ###

<pre><code>
compile(Apps::atom() | [atom()]) -&gt; ok | {error, atom()} | {error, [atom()]}
</code></pre>
<br />

Compile the configuration of the given application/s

<a name="compile_file-2"></a>

### compile_file/2 ###

<pre><code>
compile_file(In::<a href="file.md#type-filename">file:filename()</a>, Out::<a href="file.md#type-filename">file:filename()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Compile a configuration file and save the result in an other file

<a name="get_all_env-0"></a>

### get_all_env/0 ###

<pre><code>
get_all_env() -&gt; [term()]
</code></pre>
<br />

Returns the configuration parameters and their values for the application of the calling process.

<a name="get_all_env-1"></a>

### get_all_env/1 ###

<pre><code>
get_all_env(Application::atom()) -&gt; [term()]
</code></pre>
<br />

Returns the configuration parameters and their values for Application.

<a name="get_env-1"></a>

### get_env/1 ###

<pre><code>
get_env(Path::[atom()]) -&gt; undefined | term()
</code></pre>
<br />

Equivalent to [`get_env(Path, undefined)`](#get_env-2).

<a name="get_env-2"></a>

### get_env/2 ###

<pre><code>
get_env(App::atom() | [atom()] | [[atom()]], Key::atom() | [atom()] | term()) -&gt; undefined | term()
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
get_env(App::atom(), Key::atom() | [atom()], Default::term()) -&gt; undefined | term()
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

<a name="unset_env-1"></a>

### unset_env/1 ###

<pre><code>
unset_env(App::[atom()] | atom()) -&gt; ok
</code></pre>
<br />

Remove the configuration parameters and its value for the `Application` or the
given `Path`

