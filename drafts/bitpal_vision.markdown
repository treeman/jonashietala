---
title: "The BitPal vision"
tags: BitPal, Cryptocurrency
---

The Coinparty hackathon is over and our submission BitPal was well received, getting a bunch of votes by the community and even earning an honorable mention in the allstars section (sadly missing out on the $8,000 first price!)

Even though we did make something during the hackathon, that was only the first shaky steps towards what I think a cryptocurrency payment processor should be. And that's what I want to describe with this post.


# You should be in control

The big thing with cryptocurrencies is that *you* should be in full control over your payments and your money. Therefore a payment processor should have these properties:

* Open-source
* No fees
* No KYC/AML
* Full control over payment verifications (run your own node if you want to)
* Non-custodial (private keys never touch the server)


# Configurable

It's also important that a payment processor can be configured to suit your needs.

When accepting a cryptocurrency payment you as the merchant can choose how much security you want to have before the payment should be accepted. For smaller payments you might be fine with 0-conf, for larger you might want to wait for one confirmation and for very large payments you might want to wait for 6 or more confirmations.

There are also different privacy and security levels for how you verify payments:

1. **Depend on a third-party API**

   This might be fine if you process smaller payments and use a relatively trusted source.

2. **Rely on SPV security**

   With SPV security to cheat you the cheater must produce valid proof-of-work, which is a very expensive ordeal.

3. **Use a full node to verify everything yourself**

   Using a full node gives you the maximum level of privacy and security possible.

I think a payment processor should be able to provide all the different levels.

The server could provide a public REST api that you could connect to. When you outgrow it you can easily spin up a server of your own and use that access point instead. You could start with SPV security in order to save on server resources and then upgrade to a full node when you feel the need to.


# Reliable

Reliability and high uptime should be a core focus for a payment processor as downtime means payments don't go through and you'll lose customers (and money!).

This is an area that Erlang (and therefore Elixir) [shines in](https://stackoverflow.com/questions/8426897/erlangs-99-9999999-nine-nines-reliability). Some of the features that Erlang/OTP provides are:

1. **Hot code reloading**

   You can upgrade a system without stopping anything, vastly cutting down the "down for maintenance" interruptions.

2. **Compartmentalization**

   Erlang processes (lightweight threads) are isolated from each other and if one part of the system crashes, the rest lives on. Combined with message passing that allows for reliable concurrency and the OTP framework this gives Erlang a very solid base for creating fault-tolerant systems.

3. **Clustering**

   Message passing between processes works transparently between separate servers, as if they were all running on the same machine. This makes it very easy to split up the service into multiple servers. For example you could run your full node on one server and the payment processor on another or you could mirror the servers and use one as a backup if one goes down or you could run several different full node clients (like Flowee and BCHN) and combine their results.


# Extendible

I think it should be easy to extend the payment processor with the functionality you need.

With a plugin-focused architecture it should support different cryptocurrencies. The popular ones should be supported by default, and it should be easy to add new ones. This also includes adding things like SLP-tokens, shifting services or maybe Detoken for volatility control.

I'll note that I wouldn't want to officially support plugins that rely on third-parties, as there's always a risk they'll suddenly add KYC requirements or decide to exit-scam, but it should be possible for third-parties to create them.


# Advanced payment protocols

A payment isn't just sending coins from A to B, but they can include more complex interactions. For instance a Flipstarter pledge or recurring payments (depending on it's implementation).

If it's something that a merchant would like to accept, then I think a payment processor should be able to support it.


# Documentation & integrations

What sets a software library or service apart isn't always it's features, but how well documented it is and how easy it is to get started.

That's why it's absolutely essential that a cryptocurrency payment processor is well documented and is easy to start with---no matter the language or framework or server setup you use. You shouldn't have to be an expert programmer to infer the right command-line arguments to use or have an intimate understanding of cryptocurrencies to get started. It should be approachable and simple.

Examples of how to get started in popular languages (PHP/Ruby/Python/JavaScript/...), frameworks (Laravel/Rails/Django/Node/...) and e-commerce stores (WooCommerce/Shopify/OpenCart/...) is essential and it should be as easy as including some libs or copy-pasting a small code snippet.


# How do we get there?

The problem with this model is there's ...


