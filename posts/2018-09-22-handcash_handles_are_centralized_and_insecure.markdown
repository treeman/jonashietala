---
layout: post
title: "HandCash handles require trust and are insecure"
tags: Cryptocurrency
---

The [HandCash][] wallet has recently become a popular Bitcoin Cash wallet in large part thanks to their \$handle concept. It's basically a username you use instead of the normal Bitcoin Cash address, similar to how domain names abstract away IP addresses.  For example instead of sending BCH to my address `bitcoincash:qr6nm0wrkqata3klatmeuzqkjjvu725rkslzsahs8f` you can instead send it to `$treeman`. Much more user friendly, right?


# Third party trust

However there are problems. The \$handle to address mapping is kept on the wallet developers' own centralized servers. This means they could reroute all payments to any handle to some other address (unless the sending wallet has already cached the address).

This is a huge risk and central point of failure which in my opinion doesn't belong a decentralized payment system. When you pay to a \$handle you're basically trusting them not to steal your payment and that their servers are secure. The whole point of a cryptocurrency is to remove reliance on a third party which is reintroduced by \$handles.

They've said they'll open up the handle API so other wallets could use them but we should instead come up with a decentralized username system. Or try to find other ways of communicating addresses instead of having to type them in manually. For example via NFC (which HandCash also has) or QR codes (which have been used since forever).

A side note on the trust issue: *the HandCash wallet itself is also closed source*. I would never use a closed source wallet to store my coins and neither should you.


# No checksum or identification

What happens if you type `$treman` instead of `$treeman`? You will send your payment to someone else.

The Bitcoin Cash address has a checksum built in, similar to credit card numbers, social security numbers and virtually everything else you usually type in by hand. For good reason since it's very common to fat finger and mess up. This isn't something handles can provide.

Instead of checksums some systems they require confirmation with the receiver's identification. For example Swish, a popular Swedish payment system, allows you to send a payment connected to a phone number. They then display the receiver's name before you confirm the payment.

You could think it's possible modify handles to do something similar. Maybe I could attach my name to `$treeman` so you know you're sending it to me? But where Swish uses Sweden's national identification system BankID Bitcoin Cash should be a permissionless and trustless system. There is nothing preventing someone from attaching my name to `$treman` and pretending to be me. The problem is similar to look-alike domains which is fundamentally hard to solve in a decentralized handles context.  Compare it with normal addresses where it's practically impossible to generate look-alike addresses.


# Conclusion

While I like the idea of simplifying addresses and making them more user friendly there are large problems with HandCash handles. Having to trust a third party for handle resolution and them being less safe to type makes them much less secure than normal addresses and should only be used for small amounts.

Instead we could focus on alternatives to avoid having to type anything at all. For example QR codes, NFC or bluetooth could help bridge the gaps. The one example where I still find friction is when sending coins from my desktop wallet to my phone wallet which I work around by copying my address and saving a email draft and visually inspecting it before sending.


[HandCash]: https://handcash.io "HandCash Bitcoin Cash wallet"

