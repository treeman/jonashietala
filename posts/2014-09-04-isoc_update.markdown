---
layout: post
title: "ISOC update"
tags: IDA Summer of Code, Rust
---

I have now worked a bit more than 3 weeks out of my 4 weeks of [IDA Summer of Code][isoc] and this is an update post of what I've done so far. I will write a more extensive summary post at the end of the project. I was planning on writing a weekly summary, but that ship sailed a long time ago.

I've written a lot of documenting code examples, specifically making sure most of the methods in [collections][doc:col] had examples. I'm especially proud of the main examples for [priority_queue][doc:pri] and [rand][doc:rand] I wrote about [here][blog:pri] and [here][blog:rand].

I have one [RFC][] where I propose to introduce a new keyword `newtype`. I don't think it will be accepted, but it spurred some good discussions and *something* similar might be included further down the road. It remains to be seen if it will be with a new keyword or if we'll utilize existing constructions.

Other than that I've done a bit of triage: the act of looking up old issues to see if they are relevant, update them and preferably fix them. I have done some small fixes to a couple of issues, most have been very minor ones like adjusting error messages or adding tests.

The time I have left I will try to fix more issues. This week I tried to add [default arguments for json decoding when using `Option`][json decode], but I approached it the wrong way. I may attempt to do it "the right way" but I'm unsure if I can come up with a nice way.

[isoc]: /blog/2014/06/11/isoc/ "IDA Summer of Code"
[RFC]: https://github.com/rust-lang/rfcs/pull/186 "Introduce a newtype keyword"
[doc:col]: http://doc.rust-lang.org/collections/index.html "rust collection doc"
[doc:pri]: http://doc.rust-lang.org/std/collections/priority_queue/index.html "rust priority_queue doc"
[doc:rand]: http://doc.rust-lang.org/std/rand/index.html "rust rand doc"
[blog:pri]: /blog/2014/07/23/dijkstras_algorithm/ "Dijkstra's algorithm"
[blog:rand]: /blog/2014/07/30/monty_hall/ "Monty Hall"
[json decode]: https://github.com/rust-lang/rust/issues/12794 "rust issue #12794"
