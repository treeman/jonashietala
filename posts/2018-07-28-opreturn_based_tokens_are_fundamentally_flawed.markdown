---
layout: post
title: "OP_RETURN based tokens are fundamentally flawed"
tags: Cryptocurrency
---

Two new Bitcoin Cash token solutions have recently been suggested:

* [Simple Ledger Protocol: A token system for Bitcoin Cash](https://docs.google.com/document/d/1GcDGiVUEa87SIEjrvM9QcCINfoBw-R7EPWzNVR4M8EI/edit)
* [Introducing Colored Coins: a Bitcoin Cash token implementation](https://www.yours.org/content/introducing-colored-coins--a-bitcoin-cash-token-implementation-faa5cb7308c6)

Both are improvements over Tokeda as they allow permissionless transfers. However all commit to `OP_RETURN` which unfortunately is fundamentally flawed.

# No true SPV wallet support

Despite claims of being "SPV friendly" true SPV wallets cannot validate the transactions. Instead they need choose between

* Reduced SPV wallet security
* Use light wallet based validation
* Trusted 3rd parties validation

In some combination.
The whole point of SPV is to gain transaction security without trust and without validating the whole (or parts of) the chain. This removes the bases of SPV security in some ways and to be frank calling them "SPV friendly" is disingenuous.

I am also confused over scaling concerns for on-chain proposals while they're missing for proposals that suggest turning SPV wallets to light wallets. It's frankly much, much easier to grow the blocksize and let the few miners and full nodes handle the brunt work than to require SPV wallets to collect, scan or store more data. Functioning SPV wallets is the key to (trustless) scaling in Bitcoin Cash and must be treated as such. Of course we must assume popular tokens may be used proportional to BCH so they have the same scaling requirements.

# Alternative consensus model

Since `OP_RETURN` can contain both valid and invalid token transfers full nodes are used to keep track of all tokens. Put in another way instead of miner consensus we have consensus by full nodes. Or as reddit user insette eloquently put it:

> muh full node IS the consensus

Why does it matter? Well what happens when nodes disagree? Naturally this problem is what Bitcoin originally solves with POW, often referred to as Nakamoto Consensus.
For example if there's a security issue or bug in one wallet/node implementation a silent consensus divergence can occur. Compared to a Bitcoin hard fork this may not be immediately evident.

How should upgrades be done? What if the issuer wants to reverse some transactions and releases their own `OP_RETURN` interpretation implementation? What if this happens to several tokens, how should we follow the consensus of everything at the same time?
If a divergence does happen how can we decide which state is correct? "The issuer decides" isn't good enough since two users may have had problems trading amongst themselves.

Not to mention malicious actors who can try to attack the network in various ways. Defrauding users or destabilizing the network is much easier with shaky consensus.

# Fear of the Hard Fork

What is this, 2017? Arguments that a Bitcoin Cash token scheme should avoid consensus changes are exactly like the arguments that brought us SegWit instead of a regular blocksize increase. I thought we were past things like that.

Yes changes needs to be done carefully but let's take a step back. We're talking about foundations for tokens potentially worth a whole lot of money. Great care must be used regardless of solution. Suggesting to rush an off-chain solution because a hard fork one can't be ready this autumn is disingenuous and irresponsible.

# Conclusion

Work on these proposals probably started a while ago and people are understandably reluctant to set them aside, nobody wants their work to be wasted (I don't believe it is but it may feel like it if it's not adopted). Yet I think token schemes based on `OP_RETURN` is a dead end and we should focus our energy on miner validated and fully SPV capable tokens. Thus far only GROUP fits the bill.

To once again quote Counterparty contributor insette:

> But I'm just saying, there are technical reasons why OP_RETURN protocols suck, and there are also technical reasons why Ethereum doesn't suck.
