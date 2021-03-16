---
title: "An Elixir based payment processor for the Coinparty hackathon"
tags: Coinparty, Cryptocurrency
---

<div style="width=800px;height=200px;background-color=green;padding=5px">
  ![](/images/coinparty-full.svg)
</div>

Bitcoin Unlimited is hosting [a Bitcoin Cash hackathon this December][coinparty] and it seemed like a good opportunity for me to explore how to program against Bitcoin Cash, and it gives me an excuse to build something real with [Phoenix][] and Elixir. (While an event like this might help combat my procrastination, I should really work on finishing my book, so this in itself is also procrastination.)

Therefore I'd like to integrate BCH payments into a simple website made with the [Phoenix Framework][Phoenix], with the possibility of extending it to a generalized cryptocurrency payment processor (if it's sufficiently useful).

I'm also doing it myself because after entertaining my two kids, my girlfriend and spending time at work there's not that much time for the hackathon, so I figured it's best if I focus on something small and do it in my own pace.


# My use-case

Ever since I started working on [my book about cryptocurrencies][whycrypto] I've been planning to sell the digital version on my site, payable with cryptocurrencies of course.

To make it as accessible as possible I want to support different cryptocurrencies and sell it in a pay-what-you-want scheme (but more than 0). As you can send really small amounts of money this would allow people in poor countries to buy the e-book, people who otherwise would've been priced out.

It would also act as a good introduction for people who have received small amounts of cryptocurrency tips. For instance it's common on Reddit to tip \< $1 of Bitcon Cash via [chaintip][chaintip], but it’s harder to find anything useful to do with that amount of money.

When looking for a payment processor or a library to help me out, I have a few requirements:

* Easily integrated to a site made with the [Phoenix Framework][Phoenix]
* Supports multiple cryptocurrencies
* Open-source
* Free
* No minimum payment limit
* No KYC/AML
* Receive payments directly to my own crypto wallet(s)
* I should be able to verify payments via full nodes on my own hardware

  If you handle higher value payments or have high privacy requirements this is a must, but for my simple use-case I don't really have to. I just *want to*.


# I couldn't find an existing solution that fits all my needs

Turns out this is surprisingly hard. While it's possible my DuckDuckGo and Google skills have failed me, I couldn't find anything that fit my needs and they all require me to compromise on something. For instance:

6. [BitPay][]

   If I was a normal merchant I might prefer to get my payments in fiat, but it doesn't work with my requirements at all.

1. [PayButton][]

   Makes it very easy to accept Bitcoin Cash. Just add a few lines of JavaScript and you're done. But it doesn't allow you to run your own node.

2. [bchwallet][]

   Does nearly everything technical I want to support, I'd just have to integrate it into my site in some way. Unfortunately it only supports Bitcoin Cash.

   (I might use this under the hood, but I haven't done enough research to know if I need to or if I can just interface with a node directly.)

3. [BTCPayServer][]

   Many say this is the gold standard of accepting cryptocurrency payments. It does everything I want (and more!). Writing a custom integration shouldn't be too hard either.

   Although "BTC" in the name might lead you to believe that it only supports Bitcoin, it does support a number of other cryptocurrencies, such as Litecoin, Monero and even Bitcoin Gold...

   But it doesn't support Bitcoin Cash.

   Why?  It seems the maintainer has [bought into the "BCash" propaganda][btc-bcash2]:

   > I have my response. no support for it. And I will not accept PRs until all BCash wallets stop considering Bitcoin addresses as valid. (Not an help desk) Then finally I will accept PRs on the condition that the name "Bitcoin Cash" never appear. (BCH or BCash only)

   Note the double standard of supporting Bitcoin Gold and Bitcoin Plus even though they both consider Bitcoin addresses as valid and they both have "Bitcoin" in their name, which is also used [in their documentation][btcpayserver-altcoins].

   And then there's professional comments like these:

   ![[The market has spoken!][btc-bcash1]](/images/btcpayserver-bcash.png)

   Sigh.

4. [CryptoWoo][]

   This is a nice plugin that supports a variety of different cryptocurrencies, and it would fit except it's not open-source, it's not free as I want to support XMR and it only works in WooCommerce.

5. [NOWPayments][]

   Easy integration and supports lots of coins, but they have a minimum payment amount of $3-5, they have KYC/AML and I can't use my own nodes.


# Why is this important?

Now my little use-case isn't important in the grand scheme of things, but at the same time it is. So let's take a step back for a sec to see why.

People are currently gushing over Bitcoin's price again and are asking the question *when will cryptocuriencies win?* At $50,000 per coin? At $100,000? Or perhaps at \*gasp\* $1 million?

I think this misses the mark, and it's not ambitious enough.

*The end goal isn't a fiat price, it's not having to care about the fiat price.*

I want to be able to receive my salary, pay my bills and buy the stuff I need using crypto, without having to go back to fiat. In short I think cryptocurrencies win when they're as widely accepted as PayPal or VISA.

And to get there we need as many merchants as possible to accept crypto. Which means it must be as easy as possible to accept them for your business; regardless if it's in-person or online, what tech is used to build your website or whatever requirements you might have.

That's why it's so important to remove friction of any kind from the process of accepting payments. And that's why even my little project is important.


# A simple plan for the hackathon

Given that I don't have a lot of time, and I haven't programmed against cryptocurrencies before and I'll also use a language and framework I have little experience with, it makes sense to have a modest goal.

My goal is simply to accept BCH payments in a small demo website, and verify them using my own full node.


# Growing to a fully fledged project

If it makes sense to do so I might develop the project further after the hackathon, and I do have a tentative plan on how to do that:

1. Refactor it out into an Elixir library

   To make it easy to include it into a new Elixir/Phoenix website. It would be great if I could do this during the hackathon as well.

2. REST API

   On top of the library we can build a REST API so that we can integrate payments into other frameworks or e-commerce stores that don't use Elixir.

2. Documentation

   A boring---but very important---part of any API or service is documentation. People think that features is what differentiates the good from the bad, but frequently I find quality documentation is the decider.

4. More integrations

   I'll start with an integration for Phoenix, because that's what I prefer. But there are tons of general frameworks such as Ruby on Rails, Django and Laravel, or e-commerce stores like OpenCart and WooCommerce. The goal must be to support the popular ones to make it as easy as possible to start accepting payments.

   And it should be easy to write your own custom integration if native support is missing or include a small JavaScript snippet à la PayButton or Stripe.

3. Self-hosted and public servers

   It should be easy to spin up a server of your own, so you have full control of the payment processing. But everyone don't want or need to do this, and for them we should have public servers they can connect to.

4. Support more cryptocurrencies

   This is important as merchants would otherwise be forced to integrate multiple payment processors for the different cryptocurrencies they'd like to support. This is very cumbersome and they might instead opt to use BTCPayServer---that lacks Bitcoin Cash---or skip cryptocurrency support entirely.

   The fight for adoption isn't a zero-sum game between competing cryptocurrencies---it's a fight to convince the rest of the world that they should start using cryptocurrencies.

Under these larger goals there are many details missing. Enhancing 0-conf security using double-spend proofs is one example, supporting different payment protocols is another.

While this project could grow massively in size, I will start small and expand if it's valuable to do so. But the plan for the future is here.

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
[coinparty]: https://coinparty.org/
[chaintip]: https://www.chaintip.org/
[btcpayserver-altcoins]: https://docs.btcpayserver.org/Altcoins/
[BitPay]: https://bitpay.com/
