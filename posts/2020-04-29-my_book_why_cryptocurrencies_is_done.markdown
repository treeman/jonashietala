---
title: "My book 'Why cryptocurrencies?' is done"
tags: Cryptocurrency, Why cryptocurrencies?
---

My first commit on my book [Why cryptocurrencies?][main] is from Dec 19, 2018, and now about 17 months later and 1006 more commits I'm finally done!

Well, I'm done with the online version at least. I still plan to create an e-book, a PDF and a paperback and who knows how long that will take? It would be great to have a physical copy in my hands in 2020, but as I've never done these things before I don't know if that's a realistic expectation or not. Especially as I only work on it a little here and there---we did get our second kid in February after all!

Anyhow it's [available online for free][main] and I'm actually quite proud of what I've accomplished. It doesn't really matter if it's not successful and if I won't get financially rewarded for the time I've spent, because I did my best and I think it came out nicely.

![A screenshot of the chapter "What is money?"](/images/whycrypto/ex_money.png)

I didn't track exactly how much time I spent writing, I find it difficult to track my time on such a granular level, but I've tried hard to write an hour every weekday before work. I even started working 80% at my job so I could focus on writing a bit more and to be home with my family more.

And on that note working 1--2 hours less, which if you work from home and don't have to commute can turn into 2--3 hours, is **absolute gold**. I honestly don't know if I could ever go back to working full-time with a 30 min commute as I did before, the difference in quality of life is just so great.

At first I tried to work 4 days a week, and use the fifth to focus on writing, but I didn't really feel it worked well for me. Sure I was very productive the day I focused on my project, but then I had to wait a whole week until I could do it again, and then it felt quite difficult to get my mind to it again. Of course I could've written before or after work as well, but my brain is just exhausted after working 8 hours so in practice it was too difficult to keep up.

It worked *much better* to always do something, even if it's just small and if I just spent 30 min or even 15 min each day writing, I felt like I could go much further than when I tried to write in big batches. I know this isn't unique and "write a little every day" is a very common advice for writers, and for good reason I'd say.

As a final note on word count: I never counted how many words I wrote each day, how many I had written and I never had a target I was aiming at. I just tried to write down what I had to say, without padding it too much or cutting my message short.

As I wrote my book in [Pollen][pollen], which mixes text with markup and source code, it's not so easy to get a reliable word count. When counting the output html files with `sed "s/<[^>]*>//g" eli5.html | wc -c`{.fish} I come up with these numbers:

```
 31578 how_do_cryptocurrencies_work.html
 29690 uncensorable_donations.html
 29602 private_money.html
 26867 a_defective_system.html
 25799 financial_crisis.html
 23652 voting.html
 23361 what_is_money.html
 21181 challenges.html
 19292 protection_against_government_confiscation.html
 17044 timestamping_service.html
 16843 global_currency.html
 16628 for_the_unbanked.html
 16595 cryptography.html
 16401 the_blind_leading_the_blind.html
 15534 cashless_dystopia.html
 15415 cheaper_faster.html
 15151 are_cryptocurrencies_money.html
 14922 swiss_bank_account_in_your_pocket.html
 13174 darknet_markets.html
 12088 undesirable_businesses.html
 11909 tokens.html
 10850 separation_of_money_and_state.html
  9837 provably_fair_gambling.html
  9733 uncensorable_twitter.html
  8868 properties_of_a_cryptocurrency.html
  8641 freezing_of_merchant_accounts.html
  8418 about_the_book.html
  7507 look_out_for_snake_oil.html
  6403 extensions.html
  4383 about_me.html
  3885 brave_new_world.html
  3849 eli5.html
  3138 bitcoin_whitepaper.html
  2933 how_to_use.html
  2696 better_currency.html
  2396 free.html
  2360 better_digital_payments.html
  2186 what_is_a_cryptocurrency.html
  1323 index.html
  1309 acknowledgements.html
   845 appendix.html
   712 error.html
514998
```

So according to this the book is **514 998** words long. That's... really a lot. Advice on the internet seems to suggest around 100 000 words is the upper recommended limit for most, so I have a hard time believing I'm doing this correctly. This way of counting overestimates it, but I haven't bothered to figure out how much. I guess I'll see when I try to actually make a book or a PDF out of it.

How much source code did I write? That's also difficult to say, as some of it is mixed in the chapter files, but for the pure code files this is the result:

```{.fish}
$ wc -l **.rkt
...
1735 total
```

```{.fish}
$ wc -l **.scss
...
1702 total
```

So around 1735 lines in Racket and 1702 lines in Sass. Kind of interesting how the styling corresponds to almost as much code as the layout formatting does.

My experience with the technologies I used has been quite positive, but I plan to write about it more in detail in a future post.

[main]: https://whycryptocurrencies.com/ "Why cryptocurrencies?"
[pollen]: https://docs.racket-lang.org/pollen/ "Pollen: the book is a program"
[racket]: https://racket-lang.org/ "Racket"
[sass]: https://sass-lang.com/ "Sass: CSS with superpowers"

