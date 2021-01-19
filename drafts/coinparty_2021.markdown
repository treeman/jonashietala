---
title: "A self-hosted payment processor in Elixir for the Coinparty hackathon (take two)"
tags: Coinparty2021, Cryptocurrency
---

Ever since I started working on [my book about cryptocurrencies][whycrypto] I've been planning to sell the digital version on my site, payable with Bitcoin Cash of course.

I wanted to support a pay-what-you-want scheme and I wanted to verify payments with my own hardware, but I couldn't find a payment processor to fit my needs. This is why we created BitPal.

# Features

* Open source, [see the code on GitHub][src]
* No fees
* No KYC
* Non-custodial (the server does not see your private key)
* Self-hosted (using [Flowee][])
* Leverages [double-spend proofs][] for enhanced 0-conf security
* Flexible enough to truly customize the payment experience


# Try it out

There's a demo website at <https://bitpal.dev> you can try out. It uses the library to implement a simple web app and accept payments and it also includes the source code of the page.


# Howe we built it

The library is written in Elixir and the demo page uses the [Phoenix Framework][Phoenix]. We directly interface with the Bitcoin Cash blockchain via [Flowee][].

And then we used some elbow grease to figure everything out as we had very little experience with Elixir, Phoenix or interfacing with Bitcoin Cash.


# Declarations

The project was created from scratch using public Elixir libraries and uses some icons from [Refactoring UI][rui].


# What's next

The plan is to grow this to a fully fledged project. There are many improvements to make and features to add, for instance:

* Refactoring and hardening  
  As with all things created during a hackathon there are tons of rough edges that needs to be taken care of.

* Create a REST API  
  On top of the library we can build a REST API so that we can integrate payments into other frameworks or e-commerce stores that don’t use Elixir.

* Documentation  
  A boring—but very important—part of any API or service is documentation. People think that features is what differentiates the good from the bad, but frequently I find quality documentation is the decider.

* More integrations  
  The goal is to have integrations for the popular languages and frameworks, to make it as easy as possible for people to start accepting Bitcoin Cash.

* Self-hosted and public servers  
  It should be super easy to spin up a server of your own, so you have full control of the payment processing. But everyone don’t want or need to do this, and for them we should have public servers they can connect to.

* Support more cryptocurrencies  
  My goal with BitPal is to make it the one-stop-shop of accepting cryptocurrency payments, regardless of what needs you may have. And this includes accepting other cryptos too.

To make this vision a reality we would need some funding source going forward. If that would come from donations, offering premium support or Flipstarters is yet to be decided.


# Support our project

**Vote for us:** `simpleledger:qqjga3285te7ddwkgtl4ykmd83xxjte9lqjk62vke4`

**Donate to the project:** `bitcoincash:qqpkcce4lzdc8guam5jfys9prfyhr90seqzakyv4tu`


[whycrypto]: https://whycryptocurrencies.com/ "Why Cryptocurrencies?"
[src]: https://github.com/bitpal/bitpal_umbrella "BitPal source code on GitHub"
[Flowee]: https://flowee.org/ "Flowee"
[Phoenix]: https://www.phoenixframework.org/ "Phoenix Framework"
[double-spend proofs]: https://gitlab.com/snippets/1883331 "Double spend proofs - spec"
[detoken]: https://detoken.net/ "Detoken"
[rui]: https://refactoringui.com/ "Refactoring UI"

