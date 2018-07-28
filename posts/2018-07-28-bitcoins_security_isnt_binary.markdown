---
layout: post
title: "Bitcoin's security isn't binary"
tags: Cryptocurrency
---

I'd like to address a misconception that's at the core in many Bitcoin discussions lately: Bitcoin's security isn't binary. In fact security in general isn't black and white. It's a trade-off being secure enough for your threat model vs the cost and feasibility of your protection.

Consider having your front door locked for example. You could consider your home secure if your door is locked and insecure if it's open but that doesn't say what you're secure against. It's missing context. It might prevent opportunistic thefts but a more determined attacker could instead break a window, pick the lock of the door or simply break down the door itself. Maybe you live in a neighborhood with lots of thefts and you might want an alarm system, a dog or iron bars over your windows.

At the same time it might not be practical to go all out and get the best protection possible. You might not want to give up your life and move to a bunker where your friends can't visit and a private security force may be slightly too expensive.

In practice locking your door is good enough for many people. Some people do more and there are many options for them including private security for celebrities. That's okay since threat models and feeling of security is different for everyone. This is the case for Bitcoin as well. Some sell houses for Bitcoin and have high security requirements while others sell coffee and have less. Unfortunately people, including developers, don't acknowledge that Bitcoin is more fine grained than "secure" and "insecure".


# Double spend protection & Confirmations

An essential security feature of Bitcoin is double spend protection. It solves it using Proof-of-Work where each block containing a set of transactions is expensive to make and each block build on the previous block. If you want to reverse a transaction to double spend it you need to redo the work of all blocks until you find the transaction. This makes a transaction deeper in the blockchain, with more confirmations, more secure than one with less confirmations since an attacker would need more work to reverse it.

This maps well to different security requirements. One confirmation is good enough for small value goods but exchanges might want to wait six confirmations instead, which has been the default recommendation for the paranoid a while.

[A double spend attack was used in the wild to reverse transactions 22 blocks deep in Bitcoin Gold in May][btg-51]. Bitcoin isn't as easily attacked since there's not enough mining power mining other coins that can simply be temporarily rerouted for the attack but it demonstrates the attack nicely.

At the time of writing a miner is rewarded 12.5 BTC for each block found, approximately $80 000. For an attack to be worth it the double spends need to outweigh the costs of leaving out potential mining profits. This makes all but very large sums safe enough to accept with a single confirmation.

If you want to learn more I heartily recommend [the whitepaper][whitepaper] which is quite easy to read.


# 0-conf

Transactions without a single confirmation, abbreviated 0-conf, by definition aren't included in the blockchain and don't get the double spend protection confirmed transactions do. That makes them completely insecure and should never be used, or that's what some opponents tell you.

Firstly all physical goods purchased online have an inherent delivery delay. The worst that can happen if you accept a 0-conf transaction that gets double spent is you have to cancel the order. Instead if you wait for a confirmation, which on average takes 10 min but might be up to an hour, you introduce an annoying wait time for your customers.

But what's really annoying is when you want to buy things in physical stores. It might be okay to wait for a confirmation email for some online purchases but it's completely unacceptable to have to wait 10-60 min for your coffee until your transaction is confirmed (this example is always brought up). Someone could indeed double spend and steal a coffee. That's bad but it's actually not too different from what already have today. VISA allows charge backs [up to 120 days][chargeback-time] and [charge back fraud][] is a common problem for merchants. If you accept cash you risk getting counterfeit bills and you always run the risk of plain old shopliftig.

Some claim it's very easy to double spend 0-conf. [Here's a tweet][todd-rbf] from Core developer Peter Todd:

> Why opt-in RBF doesn't change zeroconf security; its already trivial to send an easy to doublespend tx.

That's only true if you naively accept 0-conf. There are strategies you can use to minimize the risk of a double spend which defeats trivial double spend scripts Peter is referring to. For example:

* Silently sample random nodes to increase the confidence that the transaction has been propagated through the network
* Wait a few seconds and look for double spends
* Require a fee that's probable to be accepted in the next block
* Use external identification like security cameras or ID cards to dissuade fraud
* For high risk transactions instead wait for confirmations

Research on how to improve 0-conf is ongoing. See for example:

* [Double-spending Prevention for Bitcoin zero-confirmation transactions](http://users.encs.concordia.ca/~clark/biblio/bitcoin/Karame%202015.pdf)  
* [Misbehavior in Bitcoin: A Study of Double-Spending and Accountability](https://eprint.iacr.org/2017/394.pdf)

With these heuristics accepting 0-conf is a viable alternative for some merchants. It's not completely secure but it's not completely insecure either. After all the competition isn't much better and the benefits are huge, waiting a couple of seconds vs 10 min. It's also completely up to the merchant's discretion if she wants to use it or not.

Despite this Bitcoin Core developers have treated 0-conf as insecure and have adopted policies that actively makes it worse. Full blocks, where all transactions compete in fees to be included, makes 0-conf unreliable because fees cannot be predicted with confidence. In December fees grew to $60 but it simply wasn't a priority to increase the block size so one can assume full blocks fits the developers' plan for Bitcoin and 0-conf does not.

They also adopted [RBF][] in the Bitcoin reference client Bitcoin Core. It's a convenient method to alter the fee *and outputs* of an unconfirmed transaction. As it's implemented in the reference client miners and wallets naturally supports it. In other words they made double spending more user friendly than before instead of adopting policies that would make 0-conf more secure "because it's already insecure".

# SPV & full nodes

Jonald Fyookball wrote a good [article on SPV security][SPVsecurity], I suggest you read it if you haven't already.

In short an SPV wallet avoids the need to download and store the whole blockchain just to be able to send and receive transactions. Basically all mobile wallets work this way. An SPV wallet is indeed less secure than a full node but it's not completely insecure (again, security is on a spectrum).

What an SPV wallet does for you:

* Ensures your transactions are in a block
* It tracks the Proof-of-Work of the blocks, i.e. follows the longest chain

What it doesn't do for you:

* Validates transactions inside a block

This means a miner can create a block with transactions that sends any number of coins from any address. That sounds bad, but realize that:

1. You can only fool the SPV wallet if you provide the longest chain
2. The miner must pay the POW cost of creating the block(s)

So to fool you the miner must, at least temporarily, outproduce the rest of the network. This isn't possible in the long run if the core security assumption of Bitcoin holds: that a majority of miners are honest.

But it's theoretically possible and thus businesses like exchanges and payment processors would still want full nodes for the extra security. [Satoshi agrees][whitepaper]:

> Businesses that receive frequent payments will probably still want to run their own nodes for more independent security and quicker verification.

Most SPV users are normal people who mostly use wallets to pay for things (SPV wallets are just as secure for the sender). If everyone had to download the whole blockchain just to use Bitcoin on the phone then I'm afraid Bitcoin wouldn't be practical as a payment system at all.

Despite SPV wallets being quite secure running a full node is presented as a requirement for everyone. Core developer Luke-Jr [doesn't even consider SPV wallet users "actual users"][luke-actualuser]:

> Running a full node makes you an actual Bitcoin user

[Here's Luke-Jr's response][luke-stackexchange] to the question "Should I use a full node as my main wallet?" on the bitcoin stackexchange:

> If you're not using a full node for your wallet, you're not using Bitcoin, and won't get the benefits Bitcoin provides over fiat currency. You might as well be using PayPal in that case, except with a random anonymous person in place of a regulated company...
>
> So, always use a full node.

That everyone should use a full node is used as an argument against raising the 1MB block size limit because there may be some people somewhere who can't store or download the data required. Despite this making Bitcoin unable to scale past 7 tx/s and fees of $60 pricing out the very people who have the most difficulty running full nodes.

# Hard forks & SegWit

A common defense in these discussions is "But always prioritizing security over all else is good". Except it's not consistently prioritized.

I bring you the case of SegWit, a solution to the [transaction malleability problem][]. Jaqen Hash’ghar wrote a [great article (2016)][A fork too far] of the whole ordeal. I recommend it although it's a bit long.

To make a long story short SegWit was deployed as a soft fork instead of a hard fork. A hard fork would have been cleaner but it requires all nodes to upgrade and people must agree with the change otherwise the coin would have split into multiple coins (like the Bitcoin and Bitcoin Cash hard fork split). This was deemed extremely unwanted since consensus would be hard to get (maybe there's some parallels to the black and white security approach but that's not the point I'm trying to make).

Instead a soft fork circumvents the problem by allowing nodes to continue working as before except they won't recognize the new features. This was used to implement SegWit as a hack where non-upgraded nodes will view segwit transactions as anyone-can-spend transactions.

Peter R gave a good talk of why this is a theoretical security regression ([slides](https://www.slideshare.net/peter_r/a-segwit-coin-is-not-a-bitcoin)):

https://www.youtube.com/watch?v=VoFb3mcxluY

This regression was pointed out by others like [Peter Todd](https://lists.linuxfoundation.org/pipermail/bitcoin-dev/2015-December/012103.html):

> In its current form, segregated witnesses makes validationless mining
easier and more profitable than the status quo, particularly as
transaction fees increase in relevance.

Granted it's only a theoretical weakness that probably won't cause any problems, but it's a clear example of a binary thought process that in this case doesn't prioritize security.

[transaction malleability problem]: https://en.bitcoin.it/wiki/Transaction_malleability "Transaction Malleability"
[A fork too far]: https://medium.com/the-publius-letters/segregated-witness-a-fork-too-far-87d6e57a4179 "Segregated Witness: A Fork Too Far"

# Conclusion

There are many examples where features of Bitcoin are discarded as "insecure" without a nuanced discussion about risks and rewards. This binary thinking is actively harmful and must be called out.


[btg-51]: https://www.trustnodes.com/2018/05/24/bitcoin-gold-51-attacked-18-million-stolen-double-spends "Bitcoin Gold 51% Attacked, $18 Million Stolen Through Double Spends"
[todd-rbf]: https://twitter.com/peterktodd/status/686365181241212928 "Peter Todd on Twitter: its already trivial to send an easy to doublespend tx"
[chargeback-time]: https://chargeback.com/visa-chargeback-time-limits/ "Visa Chargeback Time Limits"
[charge back fraud]: https://en.wikipedia.org/wiki/Chargeback_fraud "Chargeback fraud"
[0-conf doublespend]: https://eprint.iacr.org/2017/394.pdf "Double-spending Prevention for Bitcoin zero-confirmation transactions"
[RBF]: https://bitcoin.stackexchange.com/questions/10733/what-is-replace-by-fee "What is Replace By Fee"
[whitepaper]: https://bitcoin.com/bitcoin.pdf "Bitcoin: A Peer-to-Peer Electronic Cash System"
[luke-actualuser]: https://www.reddit.com/r/Bitcoin/comments/5ant54/questions_about_running_a_full_node/d9i1zyt/
[luke-stackexchange]: https://bitcoin.stackexchange.com/questions/52148/should-i-use-a-full-node-as-my-main-wallet
[SPVsecurity]: https://medium.com/@jonaldfyookball/why-every-bitcoin-user-should-understand-spv-security-520d1d45e0b9

