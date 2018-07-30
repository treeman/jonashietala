---
layout: post
title: "Bitcoin Cash needs a Specification"
tags: Cryptocurrency
---

I'm not a Bitcoin developer, I'm just some guy looking in from the outside. Lately there's been a bunch of heated debates between the different Bitcoin Cash developer teams. Something that stood out from all the noise was problems with communications and a lack of specification for new proposals. Thinking about it there's no specification at all.

# A full specification

Since Satoshi released the Bitcoin protocol it's been specified by the reference client and [Bitcoin Core is still referred as such](https://en.bitcoin.it/wiki/Protocol_documentation). It's argued that this is by design as it would avoid accidental hard fork splits if a bug would deviate from the consensus rules. The drawback is that a flaw could bring the whole network down.

After the split to Bitcoin Cash there's no longer a reference client. Instead there are several different clients that follows the consensus rules. This makes the network more durable as it's less probable that a flaw could exploit all clients and instead the risk for an accidental hard fork is higher since clients may have small bugs in their consensus rules. Therefore a lot of importance needs to be placed on making sure the different clients use the same consensus rules and bugs there are discovered.

This makes the lack of a specification baffling. It's easier to find higher level bugs in a specification and clients can focus on implementing the spec instead of trying to match their implementation with each others.

It also makes it harder to create new clients. Today the best option is to just fork the code of an existing implementation and modifying it. Of course this includes importing all technical debt and potentially crappy implementations to your client. It also constrains you to use C++, if you want to create a new highly-reliable client using Erlang then you have a bunch of reverse engineering and converting to do.

Therefore there should be a full specification of the Bitcoin Cash protocol. It should be so complete you should be able to create a new client with the spec as a reference without having to read the code of other implementations.


# Improvement Proposals

It's hard to develop software with a single team and harder to develop software with multiple teams. It's much harder if improvements and changes doesn't come with a specification and essential details can only be found by examining reference code implemented in an unfamiliar code base.

In the case of consensus changes this is very dangerous and small but important implementation details are easily missed. It not only makes the changes harder to implement for the different developer teams but it's also much harder to do security reviews for.  Importantly it makes discussions, feedbacks and reviews of the changes themselves difficult for the different developer teams.

Even optional features, like block relay networks, would benefit. Otherwise we'll run into issues like some clients implementing Xthin and others implementing Compact blocks instead due to missing specs and design flaws. It would be much better if all clients would work together and utilize the same infrastructure.

Due to communication challenges between teams perhaps it might be time to collectively work on Bitcoin Cash Improvement Proposals, similar to the [BIPS of Bitcoin Core](https://github.com/bitcoin/bips), where features could be discussed? Naturally all proposals should require complete specs.

It's of course not this straight forward in practice. There will always be discussions, both internal and external, in other channels but it's still important to have a public process like this where feedback and scrutiny can be collected community wide.

It would be hugely beneficial for the development of Bitcoin Cash if the different developer teams could agree on some common process.

