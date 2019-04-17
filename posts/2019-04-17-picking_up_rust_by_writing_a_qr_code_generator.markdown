---
title: "Picking up rust by writing a QR code generator"
tags: Rust
---

I wanted to pick up [rust][] again after having used it many years ago. After around 5 years or so I didn't really know where to start?

# Approaches to picking up a language

When learning, or as in this case re-learning, a programming language there are different approaches. For example:

* Read book(s).
* Solve different kinds of programming puzzles.
* Follow a tutorial while following along with your own code.
* Make a small game.
* Using it in a real project.

I've read a lot of programming books and in general it's a very good tool to learn new things. For rust you could for example read [The Rust Programmaing Language][rustbook], which is a pretty good introduction.

During my university time I did a little competitive programming, so you'd think I'd love the idea of solving programming puzzles. For example via [Advent of Code][aoc], [Project Euler][euler] or doing [katas][]. But truth be told---I'm not that into such puzzles. I prefer making "real" projects that does something useful for me.

<aside>
As a sidenote katas in classical martial arts is a waste of time if you want to get good at fighting or self defense. You need to train against resistance in as realistic scenarios as possible, like you do when sparring. Coding katas are more useful but I personally don't enjoy them.
</aside>

Following a tutorial, like the excellent [writing an OS in Rust][os], can be amazingly useful. Both for learning a language but mostly for learning about a specific domain---like learning about operating systems.

Personally though after programming for several years, and having used so many languages, I prefer to just pick up a language and try to create something in it.


# A QR code generator

By chance I found a thorough [QR code tutorial][qr] and it got me thinking, is it a good starter project?

* It's big enough to let me explore many of rust's features.
* It's limited in scope.
* I get to learn how QR codes work, which is something I've been curious about.
* There's a tutorial, so in theory I just have to implement it.
* There are tons of reference implementations I can compare to if I get stuck.

Seems pretty good! So [I made one][rqr].

This is how you can produce a QR code in a string representation:

```rust
extern crate rqr;
use rqr::{Qr, StringRenderer};

fn main() {
    let qr = Qr::new("HELLO WORLD").unwrap();
    let s = StringRenderer::new().render(&qr);
    println!("{}", s);
}
```

Or generate an svg:

```rust
use rqr::{Qr, SvgRenderer, Color, ECLevel};

fn main() {
    let qr = Qr::with_ecl("HELLO WORLD", ECLevel::Q).unwrap();
    let s = SvgRenderer::new()
        .light_module(Color::new(229, 189, 227))
        .dark_module(Color::new(119, 0, 0))
        .dimensions(200, 200)
        .render(&qr);
    println!("{}", s);
}
```

There's also a simple cli for the above tasks:

```
> cargo run --features cli -- "HELLO WORLD"




        ██████████████        ██    ██████████████        
        ██          ██  ████    ██  ██          ██        
        ██  ██████  ██    ██  ████  ██  ██████  ██        
        ██  ██████  ██  ██████████  ██  ██████  ██        
        ██  ██████  ██  ████  ██    ██  ██████  ██        
        ██          ██    ██    ██  ██          ██        
        ██████████████  ██  ██  ██  ██████████████        
                        ████  ████                        
          ██  ████████  ████    ██████  ████  ██          
        ██  ████████  ██        ████████  ██████          
            ██  ██  ████      ██    ████                  
        ██  ████  ██      ██  ████      ████              
        ████  ████████████████  ██████  ██████████        
                        ██      ██    ██  ██              
        ██████████████    ████    ████    ████████        
        ██          ██  ██  ██    ██    ██  ██████        
        ██  ██████  ██  ████  ██    ██      ██████        
        ██  ██████  ██  ██  ██████      ██  ██            
        ██  ██████  ██    ██        ██        ████        
        ██          ██  ██████    ██████    ████          
        ██████████████    ██  ██              ██          




```

You can customize the svg output there as well:

```
> cargo run --features cli -- "HELLO WORLD" -t svg --bg '#e5bde3' \
    --fg '#700' --width 200 > hello_world.svg
```

![](https://raw.githubusercontent.com/treeman/rqr/master/src/test/hello_world.svg?sanitize=true)

The [documentation][] and overall code quality should be fairly good but as this was just a learning project I don't plan on extending it with new features.


[katas]: https://github.com/gamontal/awesome-katas
[rust]: https://www.rust-lang.org/
[aoc]: https://www.forrestthewoods.com/blog/learning-rust-via-advent-of-code/
[rustbook]: https://doc.rust-lang.org/stable/book/title-page.html
[euler]: https://projecteuler.net/
[os]: https://os.phil-opp.com/
[qr]: https://www.thonky.com/qr-code-tutorial/
[rqr]: https://github.com/treeman/rqr
[documentation]: https://docs.rs/rqr
