

# Dōteki #

Copyright (c) 2015, 2016 Bots Unit

__Version:__ 0.1.2

__Authors:__ Gregoire Lejeune ([`gregoire.lejeune@botsunit.com`](mailto:gregoire.lejeune@botsunit.com)).



## About ##

__Dōteki__ allow you to use dynamic configuration in your erlang application.


## Using environment variables ##

In your configuration file, you can declare an explicit usage of en environment variable.

To do so, you can use and atom, prefixed by `env.` :

Erlang :

```

[
  {app, [
    {key, 'env.ENV_VAR'}
  ]}
].

```
Elixir :

```

use Mix.Config

config :app,
  key: :"env.ENV_VAR"

```

You can also use a tuple where the first element is the atom `system` or `env`,
the second element is a string giving the environment variable name and the third element is
the default value to use if the environment variable is not set. The third element is optional
and if the environment variable is not set, Doteki will return `undefined`.

Erlang :

```

[
  {app, [
    {key, {system, "ENV_VAR"}}
  ]}
].

```
Elixir :

```

use Mix.Config

config :app,
  key: {:system, "ENV_VAR"}

```

With those notations, when you get the value for `[app, key]`, Dōteki will return the value of the environment variable `ENV_VAR`.

Erlang :

```

export ENV_VAR=123
1> doteki:get_env([app, key]).
% => 123

```
Elixir :

```

export ENV_VAR=123
iex(1)> Doteki.get_env([:app, :key])
# => 123

```

You can also overwrite every value in the configuration, using an environment variable. Example: if you have the following configuration :

Erlang :

```

[
  {app, [
    {key1, 'env.ENV_VAR'},
    {key2, "hello world"}
  ]}
].

```
Elixir :

```

use Mix.Config

config :app,
  key1: :"env.ENV_VAR",
  key2: "hello world"

```

If you set an environment variable `APP_KEY2` and get the value for `[app, key2]` Dōteki will return defined by `APP_KEY2`.

__WARNING__ : The environment variables overwriting a key by its path are interpreted before those declares in the configuration. So if you export `APP_KEY1`, the value returned for `[app, key1]` will be the value exported for this variable.

When Dōteki find an environment it will try to interpret it.


<table width="100%" border="0">
<tr><th>Type</th><th>export</th><th>Erlang</th><th>Elixir</th></tr>
<tr><td>Atom</td><td><tt>"hello"</tt><br /><tt>"hello:atom"</tt><br /><tt>"hello:term"</tt></td><td><tt>hello</tt></td><td><tt>:hello</tt></td></tr>
<tr><td>String</td><td><tt>"\"hello world\""</tt><br /><tt>"hello world:string"</tt></td><td><tt>"hello world"</tt></td><td><tt>'hello world'</tt></td></tr>
<tr><td>Binary</td><td><tt>"<<\"hola mundo\">>"</tt><br /><tt>"hola mundo:binary"</tt></td><td><tt><<"hola mundo">></tt></td><td><tt>"hola mundo"</tt></td></tr>
<tr><td>Integer</td><td><tt>"123"</tt><br /><tt>"123:integer"</tt></td><td><tt>123</tt></td><td><tt>123</tt></td></tr>
<tr><td>Float</td><td><tt>"123.45"</tt><br /><tt>"123.45:integer"</tt></td><td><tt>123.45</tt></td><td><tt>123.45</tt></td></tr>
<tr><td>Tuple</td><td><tt>"{a, \"b\", 123}"</tt><br /><tt>"{a, \"b\", 123}:term"</tt></td><td><tt>{a, "b", 123}</tt></td><td><tt>{:a, 'b', 123}</tt></td></tr>
<tr><td>List</td><td><tt>"[a, \"b\", 123]"</tt><br /><tt>"[a, \"b\", 123]:term"</tt></td><td><tt>[a, "b", 123]</tt></td><td><tt>[:a, 'b', 123]</tt></td></tr>
</table>



## Using functions ##

If you want to use a function, you can use those notations :

* `{fn, Module, Function}`

* `{fn, Module, Function, [Args]}`

* `{fn, Module, Function, Arity, [Args]}`

* `{'fun', Module, Function}`

* `{'fun', Module, Function, [Args]}`

* `{'fun', Module, Function, Arity, [Args]}`

* `'fun.Module:Function'`

* `{'fun.Module:Function', [Args]}`

* `{'fun.Module:Function/Arity', [Args]}`

* `'fn.Module:Function'`

* `{'fn.Module:Function', [Args]}`

* `{'fn.Module:Function/Arity', [Args]}`

Erlang :

```

[
  {app, [
    {key, {fn, module, function, 2, [{system, "ENV_VAR"}, 2]}}
  ]}
].

```
Elixir :

```

use Mix.Config

config :app,
  key: {:fn, Module, :function, 2, [{:system, "ENV_VAR"}, 2]}

```


## Licence ##

Copyright (c) 2015, 2016, Bots Unit<br />
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
1. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.


THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/botsunit/doteki/blob/master/doc/doteki.md" class="module">doteki</a></td></tr></table>

