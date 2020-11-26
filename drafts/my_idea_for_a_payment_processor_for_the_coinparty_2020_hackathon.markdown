---
title: "Building a payment processor for the Coinparty 2020 hackathon"
tags: Coinparty2020
---

Bitcoin Unlimited is hosting a Bitcoin Cash hackathon this December and it seemed like a good opportunity for me to explore how to interface with cryptocurrencies for real, and it gives me an excuse to build something real with [Phoenix][] and Elixir.

Therefore I'd like to integrate BCH payments into a simple website made with the Phoenix Framework, with the possibility of extending it to a generalized cryptocurrency payment processor (if things go as planned).

But before explaining why I think it's even necessary, let's take a step back and look at the big picture.


# The cryptocurrency end goal

Bitcoin (BTC) has recently gone up massively in price and people love to speculate on what price we might reach this time. Will we reach a new all-time high? Will be surpass it?  And above all---at what price do you think Bitcoin has succeeded? *What's the end goal?*

I think all these question miss the mark, and aren't ambitious enough.

Yes it would be great if Bitcoin reached $100,000/coin or if Bitcoin Cash would be worth more than Bitcoin, but that's not the end goal.

**The end goal isn't a fiat price, it's not having to care about the fiat price.**

I want to be able to receive my salary, pay my bills and buy the stuff I need using cryptocurrencies, without having to go back to fiat. In short I think cryptocurrencies win when they're as widely accepted as PayPal or VISA.

And for that to happen we need as many merchants as possible to accept cryptocurrencies. Which means it must be as easy as possible to accept cryptocurrencies for your business; regardless if it's in-person or online, what tech is used to build your website or whatever requirements you might have.


# My use-case

And this is where I find myself. I've [written a book][whycrypto] that's free to read online, but I'd like to put up an e-book version up for sale on my site to enable people all over the world to buy it using crypto.

To make it as accessible as possible I want to support different cryptocurrencies and sell it in a pay-what-you-want scheme (but more than 0). As you can send really small amounts of money with for example Bitcoin Cash, this would for example allow people in poor countries to buy the e-book.

It would also act as a good introduction for people who've received small amounts of cryptocurrency tips. For instance it's common on Reddit to tip $1 of Bitcon Cash via [chaintip][], but it's harder to find anything useful to do with that amount of money.


# A potential problem

The problem I have now is how best to accept cryptocurrencies?

I have a few requirements:

* Easily integrated to a custom site made with [Phoenix][]
* Support multiple cryptocurrencies
* Open-source
* Free
* No KYC/AML (meaning it must be completely non-custodial)
* No minimum payment limit
* Verify payments via full nodes

  If you handle higher value payments or have high privacy requirements this is a must, but for my simple use-case I don't really have to. I just *want to*.

Turns out this is surprisingly hard. While it's possible my DuckDuckGo and Google skills have failed me, I couldn't find anything that fit my needs.

There are a bunch of projects that do something similar, for instance:

1. [PayButton][]

   Makes it very easy to accept Bitcoin Cash. Just add a few lines of JavaScript and you're done. But it doesn't allow you to run your own node.

2. [bchwallet][]

   Does nearly everything technical I want to support, I'd just have to integrate it into my site in some way. Unfortunately it only supports Bitcoin Cash.

   (I might use this under the hood, but I haven't done enough research to know if I need to or if I can just interface with a node directly.)

3. [BTCPayServer][]

   This is the rock-star of accepting cryptocurrency payments. It does everything I want it to (and more!). Writing a custom integration shouldn't be too hard either.

   Although "BTC" in the name might lead you to believe that it only supports BTC, it does in fact support a number of other cryptocurrencies, such as Litecoin, Monero and even Bitcoin Gold... But it doesn't support Bitcoin Cash.

   Why?

   It seems the maintainer has [bought into the BTC maximalist "BCash" propaganda][btc-bcash2]:

   > I have my response. no support for it. And I will not accept PRs until all BCash wallets stop considering Bitcoin addresses as valid. (Not an help desk) Then finally I will accept PRs on the condition that the name "Bitcoin Cash" never appear. (BCH or BCash only)

   Note the double standard of supporting Bitcoin Gold and Bitcoin Plus even though they both consider Bitcoin addresses as valid and they both have "Bitcoin" in their name.

   ![That's a [nice pile of poo][btc-bcash1] you've got there.](/images/btcpayserver-bcash.png)

   Very professional.

   Not.

4. [CryptoWoo][]

   This is a nice plugin that supports a variety of different cryptocurrencies, and it would be exactly what I'd want except it's not open-source, it's not free if I want to support XMR and it only works in WooCommerce.

5. [NOWPayments][]

   Easy integration and supports lots of coins, but they have a minimum payment amount of $3-5 and they have KYC/AML.

I looked at some other solutions that I don't list here, but I just couldn't find anything that fits my needs. Therefore I'll try to plug that hole.


# Why talk about other crypto during a Bitcoin Cash hackathon?

Some might find it strange or even distasteful that I'm talking about other cryptocurrencies in a Bitcoin Cash hackathon, in an article that I'm required to submit and will surely be used during judging no less.

Naturally I plan to focus on Bitcoin Cash exclusively during the hackathon, and I will continue to do so until I can support the features I want, which will take much more time than I can spend on the hackathon. Bitcoin Cash is after all currently one of our best bets to realizing peer-to-peer electronic cash, so first-class support is a must.

But as a larger point I think it's a bad idea to rely on a payment processor that exclusively focuses on Bitcoin Cash as it will make it hard for merchants who, like me, wants to accept other cryptos and force us to do a separate integration per crypto. Then we might instead opt to use something like BTCPayServer---that lacks Bitcoin Cash support---or skip cryptocurrencies entirely.

So paradoxically I think building a payment processor that supports multiple cryptocurrencies will be more beneficial for Bitcoin Cash than building one exclusively for it. The fight for adoption isn't a zero-sum game between competing cryptocurrencies---it's a fight to convince the rest of the world that they should use cryptocurrencies.


# A tentative plan

Because I don't have a lot of time to spend on the hackathon I've decided to do it myself and I've split up the project into subtasks and I'll do as many as I can, the rest will have to wait until later.

1. The MVP

   The minimal solution is to be able to accept a BCH payment in a small demo website, without any bells and whistles.

2. Refactor out into an Elixir library/app

   It should be as easy as possible to include it into a new Elixir/Phoenix website.

3. REST API

   On top of the library we can build a REST API so that we can integrate payments into other frameworks or e-commerce stores that don't use Elixir.

4. Easy to self-host

   It should be easy to spin up a server of your own, so you have full control of the payment processing.

5. More integrations

   I'll start with an integration for Phoenix, because that's the what I prefer. But there are tons of others, such as Ruby on Rails, Django, Laravel, OpenCart and WooCommerce. The goal must be to support the popular ones to make it as easy as possible to start accepting payments. And as a fallback it should be easy to write your own custom integration if native support is missing.

Beneath all this are many details missing. Enhancing 0-conf security using double-spend proofs is one example, supporting different payment protocols is another. Documentation sounds boring, but is extremely important for something like this.

I absolutely don't expect to have time for all of this. If I manage to accept payments to a demo website, then I consider that a success for me during this hackathon. But the plan for the future is here.

> Shoot for the moon. Even if you miss, you'll land among the stars.
>
> --- Norman Vincent Peale 


[PayButton]: https://paybutton.org/
[bchwallet]: https://github.com/gcash/bchwallet
[BTCPayServer]: https://btcpayserver.org/
[CryptoWoo]: https://www.cryptowoo.com/
[Phoenix]: https://www.phoenixframework.org/
[whycrypto]: https://whycryptocurrencies.com/
[chaintip]: https://www.chaintip.org/
[btc-bcash1]: https://mobile.twitter.com/BtcpayServer/status/1031388746531332096
[btc-bcash2]: https://mobile.twitter.com/BtcpayServer/status/963686582862655488
[bip-70]: https://github.com/bitcoin/bips/blob/master/bip-0070.mediawiki
[NOWPayments]: https://nowpayments.io/

