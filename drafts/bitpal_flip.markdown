---
title: "BitPal Flipstarter draft"
tags:
---

After the [Coinparty hackathon][coinparty] where our self-hosted payment processor was well received and earned an all-stars honorable mention, it's time to develop it into a production-ready solution. With the help from the Bitcoin Cash community we hope that Flipstarter will be a way to make that happen.


# The BitPal vision

> The end goal isn’t a fiat price, it’s not having to care about the fiat price.

The unique property of cryptocurrencies is the removal of a third party. It means you can accept payments for whatever you want, without having to worry about KYC, censorship or third-party fees.

To reach this goal merchants need to process payments on their own, and that's what BitPal is all about. It's a payment processor focused on reliability and extendability where you validate payments using your own hardware.

Please see our [declaration to join the coinparty hackathon][join-coinparty] and [the BitPal vision][vision] for some background on BitPal and where we want the project to end up.

[vision]: https://read.cash/@jonas_h/the-bitpal-vision-8f038540
[join-coinparty]: https://read.cash/@jonas_h/an-elixir-based-payment-processor-for-the-coinparty-2020-hackathon-21d199a4
[vision-discussion]: https://www.reddit.com/r/btc/comments/l09k7a/bitpal_a_selfhosted_payment_processor_for_the/
[verde]: https://flipstarter.bitcoinverde.org/
[coinparty]: https://coinparty.org/
[devpost]: https://devpost.com/software/bitpal
[bitpal1]: https://www.youtube.com/watch?v=7qh006w2FZo
[bitpal2]: https://www.youtube.com/watch?v=HybG5JYmlZ8



# What you'll get

Although there are many things we'd like to work on, for this Flipstarter we're trying to focus on the most important issues in order to reach the MVP state.

It's super important to understand that this Flipstarter will fund a fixed working hours, and while we estimate that we can get these items done in that time, there's no guarantee. Time estimation is one of the hardest things in software development and it's common to widely underestimate (or overestimate) how much work a task requires.

And if we have extra time there are plenty of other features not listed here that we can start working on. For example adding an SPV configuration, to offer a more lightweight alternative to running a full node, or creating integrations for popular languages or frameworks.


## Making it production ready

While we did some good things during the hackathon (we do have [a working demo][demo] up and running) there are some fundamentals things we need to improve:

[demo]: https://bitpal.dev/

1. Refactoring & tests

   As the name *hack*athon implies, what's produced is often a bit hacky and of poor quality. That's why we need to start with improving the existing code by cleaning it up and making sure it's robust.

   If you're not a developer and if you wonder why refactoring (rewriting and cleaning up code) is needed I like this analogy:

   Refactoring code is similar to cleaning in a restaurant. In the short-term you can get away without cleaning, and you might save some time by skipping it, but in the long-term it will destroy your restaurant as your customers will get food poisoning. And the same is true for refactoring, especially if you want to keep building upon your code.

2. Plugins & adapters

   Making the payment processor extendible would allow us to easily add support for SLP tokens, add a backend for SPV security or switch out our current Flowee powered backend for BCHN. We don't plan to implement these extensions in this Flipstarter, but it's important to set the correct design early in the project.

   It may sound like a lot of work, but the implementation is already quite modular so we just have to ensure we don't have unnecessary coupling between them and define some common interfaces.

3. Modern payment support

   For the hackathon we reused a single address and separated different payments by adding a random amount of satoshis. This is fine for a proof-of-concept, but the real solution is to use the xpub key to a new address for each payment. It's also a good idea to implement the [BIP-70][] and [BIP-75][] for better user experience and security.

[BIP-70]: https://github.com/bitcoin/bips/blob/master/bip-0070.mediawiki
[BIP-75]: https://github.com/bitcoin/bips/blob/master/bip-0075.mediawiki

## A deployable REST server

BitPal is currently implemented as an Elixir library. That works great if you have an Elixir website and want to run the payment processor in the same cluster, but it's fairly useless if you want to use it with another programming language.  Therefore we'll build a REST API on top of the library which can serve any language.

While we'll host it on our domain, so you can test the API or even accept payments easily, it should be easy to deploy even for inexperienced people.


## Documentation

It's not sexy to spend time on documentation, but it's still one of the most important features of any software library or api. With BitPal we'd like to spend time to do documentation right and to make sure we have the tutorials, the how-to guides and the references to ensure that it's as easy and as painless as possible to use.

# Requested funding

We request X BCH to cover hosting costs and to fund us taking a break from other commitments to work on BitPal for Y hours over the course of Z.

## Hosting

We host BitPal on DigitalOcean and 1.5 BCH should cover hosting for around 6 months.

## Work hours

Both Jonas and Filip has many things going on and work hours must therefore be flexible. The hours will be put in, but it may happen that a lot of work is done one week but nothing is done the week after. We estimate that the project will be done around July (depending on when the Flipstarter finishes), and we'll post regular updates to help you keep us accountable.

We calculate 1 hour = 0.3 BCH.

### Jonas

Jonas will work 240 hours in total, for 72.0 BCH, with *around* 10 hours/week the first two months and after that transition to around 20 hours/week (and sometimes more) until the hours are done.

### Filip

...

# How you'll stay updated

Roughly once a month (depending on how much time we can commit to that particular month) we'll write an article describing what we've done so far and what the next steps will be. You can also follow the development [on github][github] where we'll try to keep our technical discussions, either in issues or in pull requests.

[github]: https://github.com/bitpal


# Who are we?

Jonas has been active in the BCH community long before BCH split from BTC and is an avid supporter of the p2p digital cash vision.  He's writing [a book about cryptocurrencies](https://whycryptocurrencies.com/) that aims to teach people about the usefulness of crypto as money and is active on Reddit as [/u/jonas_h](https://www.reddit.com/user/jonas_h/).

Filip ...


# How to contact us?

Please reach out to <contact@bitpal.dev>.


[good-doc]: https://documentation.divio.com/
