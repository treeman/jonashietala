---
layout: post
title: "Understanding the Computer"
category: Computer
tags: Computer, Programming
---


When I discovered programming it felt like a whole new world opened up for me with endless possibilities. It granted unlimited power and I could create virtually anything and it explained so much to me - how did a program work? How could you make a game or a website? But there was a big question without a good answer lurking in the background. I understood how you made the computer to do something and I understood the concept of electronics on a very crude level, but how did you ever go from a current to a full fledged game?


After 2,5 years in University I finally feel I can understand *a bit* of that. This is a simplified view of my journey so far.

# Basic Electronics

We had a basic electronics course, although I must admit I never really got a good understanding of it and I haven't used it enough to be comfortable with it. But I have at least a basic idea of how basic digital components can be realized.

# Basic digital components

We only had one introductory course here as well. I didn't like it at all at the time, I found it a bit dumb. Why care about the theory behind digital logic so much? Who cares?

Well, it's impossible to build something bigger without the building blocks and it's hard to build a computer without basic combinatorical networks. Still didn't like it though.

# Larger digital components

This was the first hardware based course (we had several) I liked. Our mission was to build a digital clock or something. Turns out it's really fun to build things! We extended the clock to be a counter, up or down, and we built other simple things. Life was good and the world of computers started unlocking a little, from the hardware point of view this time.

# A processor

I'm not sure how it happened but the next step after playing with the smaller digital components was to play with a real CPU - motorola's 68k family. This was also my first insight into assembly, it's something I always wanted to know. Turns out it wasn't all that different from regular programming, except that you're dealing with much more detail. We used the processor to communicate with our familiar digital hardware, we made a 1D ping-pong with diodes and a simple "sinking ship" game and a lot of small assignments. We ported (parts of) Forth to it!

After that we had a construction course, we could make a digital *thing*! Maybe a processor, hurr? [So we did][MARC]. We made a microprogrammed processor, simulated on an FPGA, which ran redcode. I even wrote an assembler for it!

# Compiler

*I'm waiting eagerly for our compiler course to start in ~10 months.*

# Operating system

We had a super fun course about operating systems and we did some work in [pintos][], an OS where you're supposed to improve and implement parts of it. It was perhaps the best course I've had yet and I learnt a *ton*. Threading, scheduling, system calls, security, file systems, launching programs, etc...

After pintos it feels like I understand what a OS does and how it communicates with hardware and programs.

# A program

Ah, we've come a whole circle. I'm still writing programs, that's practically all I'm doing, but now it feels like I have a better grasp of what it takes to run it.

There are still things I'd like to do more of. More games, more assembly code, more hardware controlling stuff, using my [newly gotten][] [raspberry pi][] perhaps?, and I definately want to make my own programming language sometime.

[MARC]: https://github.com/treeman/MARC/
[pintos]: http://www.stanford.edu/class/cs140/projects/pintos/pintos_1.html
[newly gotten]: /blog/2012/12/14/early_christmas_present/
[raspberry pi]: http://www.raspberrypi.org/

