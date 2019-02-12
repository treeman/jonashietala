---
title: "The ASIC mining trap"
tags: Cryptocurrency
---

Monero is once again [changing it's PoW][fork] following confirmation that [85% of Monero's hashrate is ASICs][monero-asics] in order to prevent ASICs on the chain. Like last time Monero bricked ASICs discussions if ASICs are good and bad have sparked.

There are mainly two sides in the debate where both groups have strong opposing opinions. I think there are pros and cons with both approaches and I don't really have a preference. I'm just glad there are projects exploring the different options.

But there's one issue I don't think is emphasised enough: The biggest problem with ASICs is in the transition from commodity hardware to ASICs.

(image)

This is just a high level description of how I see it.

Note that you can always create ASICs or mine with GPUs for any algorithm. The difference is if they out-compete other hardware to a very high degree. For example you can spin up your GPU cluster to mine on Bitcoin, but you'll lose money and won't impact anyone.

# CPU mining

A coin with profitable CPU mining is pretty good. It's weak to large botnets and supercomputers but everyone can become a miner. Even web miners can join in.

# GPU mining

A coin ruled by GPUs might be a little better as it counters the botnet and supercomputer problem while still being accessible to most people.

# FPGA mining

When FPGAs enter the picture the situation deteriorates, since people don't usually have FPGAs at home, but you can still buy them if you really want. Historically the leap from GPUs to FGPAs hasn't been very significant as the leap to ASICs.

# ASIC mining

Now the interesting thing is when ASICs are first created. The first one who makes them can get a **huge** advantage over everyone else. Now they alone have access to vastly more efficient hardware and they can earn more money or compromise the network's security.

This was the case when Bitmain first created ASICs for Bitcoin or when they made Cryptonight ASICs the last time Monero forked. This is also the case now for both Monero and Dash who are dominated by ASICs.

There's also a big weakness here against state level actors for coins striving to be ASIC resistant. A state could spin up a Manhattan Project with the goal to create efficient ASICs in order to compromise the network. If ASICs were readily available the state would need to compete against the open market which would be much harder. It would be extremely difficult to compromise the Bitcoin network this way for example.

As ASIC development for a coin continues this first mover advantage falls away. The BTC pool distribution looks pretty good to me now:

![Image from [coin.dance](https://coin.dance/blocks)](/images/2019-02-12-BTC-hashrate.png)

When we reach the point that ASICs can be bought from regular computer stores the situation has improved to around the state of a coin with primarily GPU mining.

# The current state

(image)




[fork]: https://www.getmonero.org/2018/02/11/PoW-change-and-key-reuse.html
[monero-asics]: https://medium.com/@MoneroCrusher/analysis-more-than-85-of-the-current-monero-hashrate-is-asics-and-each-machine-is-doing-128-kh-s-f39e3dca7d78

