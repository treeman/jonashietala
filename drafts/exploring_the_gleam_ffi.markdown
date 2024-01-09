---
title: "Exploring the Gleam FFI"
tags: [Gleam]
---

My brain is a curious thing.
I'm on a business trip right now and I've set aside time to finish some important todos I want and need to get done.
But instead of focusing on them, I started playing around with [Gleam][]---a young and interesting programming language.

My (current) favorite programming languages are [Rust][] and [Elixir][].
They're really different from each other, but they work well together as Rust can easily be [embedded via Erlang NIFs][rustler].
This is useful because one of the drawbacks with Elixir (or rather the Erlang VM that Elixir runs on) is raw computational performance,
something that Rust excels at.

Another drawback with Elixir is the lack of typing (although [improvements are underway][elixir-types]).
This is what drew me to Gleam, which feels like Elixir but with Rust types on top---a fantastic sales pitch.

A big drawback with young languages is that there aren't a lot of libraries out there, so you'll have to roll your own solutions most of the time.
But the beauty of targeting existing platforms is that you can also leverage their libraries,
which in Gleam's case means all Erlang and Elixir libraries (or JavaScript, depending on your compilation target).

This is accomplished via Gleam's FFI, which is quite convenient.

Let's explore.

# Erlang

```
gleam new myapp
```

As shown in [the Gleam Book][external-functions] calling standard Erlang functions is straightforward.
You declare foreign functions with the `@external` keyword and then call them as normal:

```gleam
import gleam/io

@external(erlang, "rand", "uniform")
pub fn random_float() -> Float

pub fn main() {
  io.debug(random_float())
}
```

That if run will call the `uniform` function in the `rand` Erlang module:

```
> gleam run
0.43487935467166317
```

This works for standard libraries, but you can access other Erlang libraries on [Hex][] by adding them as dependencies in `gleam.toml`:

```toml
[dependencies]
base32 = "~> 0.1.0"
```

Declare and call it like before:

```gleam
@external(erlang, "base32", "encode")
pub fn encode_base32(x: String) -> String

pub fn main() {
  io.debug(encode_base32("superhidden"))
}
```

And Gleam will download and compile the Erlang dependency:

```
> gleam run
  Compiling base32
===> Fetching rebar3_hex v7.0.7
===> Fetching hex_core v0.8.4
===> Fetching verl v1.1.1
===> Analyzing applications...
===> Compiling hex_core
===> Compiling verl
===> Compiling rebar3_hex
===> Analyzing applications...
===> Compiling base32
"ON2XAZLSNBUWIZDFNY======"
```

If you want to write Erlang code yourself and call that, it's also very easy.
The gleam compiler will compile and include `.erl` files automatically.

For example this file `src/erlib.erl`:

```erlang
-module(erlib).
-export([ping/0]).

ping() ->
    io:fwrite("ping~n", []).
```

Declare and call the function in gleam like before:

```gleam
@external(erlang, "erlib", "ping")
pub fn ping() -> a

pub fn main() {
  ping()
}
```

```
> gleam run
ping
```

You can also call Gleam functions from Erlang.
With for example this pong function in `src/mypong.gleam`:

```gleam
import gleam/io

pub fn pong() {
  io.println("pong")
}
```

You can call the Gleam function with `module:function()`:

```erlang
ping() ->
    io:fwrite("ping from Erlang~n", []),
    mypong:pong(). % Call the Gleam function
```

```
> gleam run
ping from Erlang
pong
```

# Elixir

If you want to include Gleam code into an existing Elixir project, look at [MixGleam][] on how to teach `mix` how to work with Gleam code and dependencies.

But if you want to include Elixir code into your Gleam project, you can do that just as easily as with Erlang.

Calling Elixir functions is used with the same `@external` keyword:

```gleam
import gleam/io

@external(erlang, "Elixir.RandomColor", "hex")
pub fn random_color() -> String

pub fn main() {
  io.println(random_color())
}
```

Note that Elixir modules gets a `Elixir` prefix and that we're still calling external Erlang code as Elixir gets compiled to Erlang.

Dependencies are added from [Hex][], exactly the same as with Erlang dependencies:

```
gleam add random_color
```

```
> gleam run
#3724C9
```

Calling our own Elixir code is just as easy as with Erlang.
Just include it in your source directory, like this `src/exlib.ex`:

```elixir
defmodule Exlib do
  def ping() do
    IO.puts("ping from Elixir")
    :mypong.pong()
  end
end
```

And refer to the module as `Elixir.Exlib`:

```gleam
@external(erlang, "Elixir.Exlib", "ping")
pub fn ping() -> a

pub fn main() {
  ping()
}
```

```
> gleam run
ping from Elixir
pong
```

The drawback with the Elixir integration is that you can't call Elixir macros, you need to operate on the generated functions instead,
and if you include Elixir you'll also include the Elixir standard library.
If you want something more lightweight you should probably prefer to use Erlang directly.

# Rust

In the beginning I wrote that you could easily call Rust code from Elixir via [rustler][].
You can call Rust from Gleam as well, either via Erlang or Elixir.
If we use Erlang as the glue we don't have to mess with `mix` and can keep using the Gleam toolchain.


- Call Rust code from Gleam
- Include a Rust library

# JavaScript

- Gleam compiles to JavaScript!
- Call JavaScript from Gleam
- Call Gleam from JavaScript

https://xkcd.com/221/

[Gleam]: https://gleam.run/ "Gleam is a friendly language for building type-safe systems that scale!"
[Rust]: https://www.rust-lang.org/ "Rust programming language"
[Elixir]: https://elixir-lang.org/ "Elixir programming language"
[rustler]: https://github.com/rusterlium/rustler
[elixir-types]: https://nitter.net/josevalim/status/1744395345872683471
[elixir-rustler]: https://github.com/Qqwy/elixir-rustler_elixir_fun "Calling Elixir code from Rust"
[external-functions]: https://gleam.run/book/tour/external-functions.html
[Hex]: https://hex.pm/
[MixGleam]: https://github.com/gleam-lang/mix_gleam 
