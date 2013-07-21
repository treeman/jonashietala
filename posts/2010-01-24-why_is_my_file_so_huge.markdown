---
layout: post
title: Why is my file so huge?
category: Games
tags: Games
time: 17:39:55
---
My latest game was absolutely huge! And I'm not talking about the music (which was pretty huge too - roughly 20mb) but the little .exe file.

It was 14,6mb!!

Now every way you look at it, *that's incredibly huge*. It's like comparing an ant to a human. Normally the little ant is the exe file which should be small, except that it's not.

<div class="center"><img src="http://www.kent.net/robotech/gallery/images/ant.gif" /></div>

Incredible I thought, what the heck did I do wrong? I mean my code isn't really good but I never knew it was this horrible. So today I was determined to find the cause of this obscene mutant ant.

I started out chopping off everything regarding exceptions, cause all c++ resources I've read say exceptions will take space like a mutant gremlin. Okay I thought and chopped away everything - but nothing happened.

Now that's weird, what happens if I scrap this.. and this.. It ended with me beginning a big revamp of my whole "engine", or rather collection of stuff - nothing inherently wrong as it was badly needed - but nothing happened with my exe file! It was still almost 2mb big with basically only a hello world...

Then it struck me! I had been using *-g* with gcc and without any optimizing at all. When I turned on size and speed optimizations and scrapped the debugging the change was quite extraordinary.

The mutant 14 637 kb was magically transformed to a more fitting ant size of 856 kb. I couldn't save much of the total file size (23 254 kb -> 20 768 kb) so while I apologize for hogging your bandwidth, time and harddrive space I'm hoping you won't be too mad at me.

