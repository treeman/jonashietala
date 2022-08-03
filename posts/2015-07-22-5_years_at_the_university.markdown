---
layout: post
title: "5 Years at Linköping's University"
tags: Life, School
recommended: true
---

I recently finished my master's degree (civilingenjör or civil engineering) at Linköping's University. At first it felt like 5 years would be an eternity, but in hindsight it was over in a flash. my gut feeling is that I haven't learnt or done anything of note, except you know actually finishing my degree, but if I sit down and think about it I have done some things. Actually quite a lot of things.


# First year

I moved to Linköping with my girlfriend and it was quite an adjustment as we both moved from our parents and to a city where we practically knew nothing and no one. But we managed fine and while we never went out to parties or went to other student events we found some friends. I also started [ITF Taekwon-do][saja] after a few months.

There weren't too much to do in school. The focus was on foundation math courses, mostly things I had already done but needed refreshing, and on introductory programming courses which weren't challenging at all for me.


# Second year

During the second year I felt the courses were harder and more interesting than the first year's courses. The courses in Combinatorial Optimization, Data Structures and Algorithms and Linear Algebra were fun and new for me. I also took an extra continuation course in Linear Algebra which was interesting.

When I started the university I thought I would like electronics and hardware construction, but the courses from the first year were really not interesting at all. Even when making the transition from analog to digital I didn't care for it at all. But it all changed when we started actually *building* things with digital circuits.

At first we had an introductory course where we built digital clocks and counters. You were supposed to have prepared a design sketch for the labs, but me and my partner for some reason never did it, and so we were always running late in the labs. I remember one very fun, or horrible, occasion where we had struggled a lot with one lab and we had spent almost the full 4 hours until we finally completed it with maybe 10 - 15 minutes left. Filled with relief we started taring apart what we so painstakingly had built. When done we realized we had one part left where we were supposed to use the circuit we just destroyed... Luckily we could use the circuits another group had made, but it was a painful lesson in always reading the descriptions and prepare for the labs in advance.

The digital circuit labs were followed by a digital project of our own choice. We [designed a processor][MARC] which could run the [Core Wars 88 standard][corestd] and we simulated it on an FPGA.

![The main block schema for the processor](https://camo.githubusercontent.com/629c93393e92277b309693a5a99228a50b2bcc98/68747470733a2f2f7261772e6769746875622e636f6d2f747265656d616e2f4469676974616c2d6b6f6e737472756b74696f6e2f6d61737465722f5265706f727425323025323625323070726573656e746174696f6e2f6875767564626c6f636b736368656d612e706e67)

And here is a small video of the simulation in action:

https://www.youtube.com/watch?v=E2sehbjNtDg

The project was interesting as it was an outlet for our creativity and it touched on several different areas. Processor design, hardware debugging (!) and I wrote a [redcode assembler][] for our CPU and a [microcode compiler][].

The behaviour of the processor is controlled by microcode which basically is a set of control outputs of 0s and 1s. For example our processor has 18 outputs consisting of a total of 39 bits (`uPC_addr` is internal).

```
game FIFO IR ADR1 ADR2 OP M1 M2 mem1 mem2 mem3 mem_addr ALU1 ALU2 ALU buss PC  uPC  uPC_addr
 00   00  0   00  00   0  00 00  00   00   00    000     00   0   000 000  00 00000 00000000
```

To make things easier, instead of manually editing each 0/1 line we introduced a DSL which then the compiler turns into microcode lines.

For example [the microcode][] is described in our DSL:

```
; Startup, check if we're in game
        jmpS $GAME                              ; Execute game code only if we're running
        jmpO +0                                 ; Infinite loop if we've recieved game over, reset to break it

; Clear memory contents
        ALU = 0                                 ; Load 0
        ALU1 -> buss, buss -> OP, buss -> M1, buss -> M2, buss -> PC
:CLRMEM PC -> mem_addr                          ; Look at PC
        OP -> mem, M1 -> mem, M2 -> mem         ; Clear it
        ALU++                                   ; Incr
        ALU1 -> PC, jmpZ $LOADP                 ; If 0 we're done looping
        jmp $CLRMEM                             ; Else continue
...
```

And is then compiled into [VHDL code][]:

```vhdl
signal mem : Data := (
    -- Startup, check if we're in game
    "00000000000000000000000000000000000011000110011", -- jmpS GAME(33)
    "00000000000000000000000000000000000101000000001", -- jmpO +0(01)

    -- Clear memory contents
    "00000000000000000000000000110000000000000000000", -- ALU = 0
    "00000000010101000000000000000100010000000000000", -- ALU1 -> buss, buss -> OP, buss -> M1, buss -> M2, buss -> PC
    "00000000000000000000110000000000000000000000000", -- PC -> mem_addr
    "00000000000000101010000000000000000000000000000", -- OP -> mem, M1 -> mem, M2 -> mem
    "00000000000000000000000000100000000000000000000", -- ALU++
    "00000000000000000000000000000100010001100001001", -- ALU1 -> buss, buss -> PC, jmpZ LOADP(09)
    "00000000000000000000000000000000000001000000100", -- jmp CLRMEM(04)
...
```

The redcode assemblers job is to transform a warrior (a piece of redcode) into a binary object file which we can then send to the FPGA for execution.

For example this warrior:

```
step    EQU 417
init    EQU 1337
size    EQU 9

        JMP start           ; boot jump as we can't specify PC in the middle T.T

src     DAT 0               ; src pointer
start   MOV #size, src      ; setup src pointer
copy    MOV @src, <dst      ; copy self
        DJN copy, src
        SPL @dst            ; throw a pc there

        ADD #step, dst      ; space out a bit
        JMP start           ; make a new copy, yay!

dst     DAT #0, #init       ; dst pointer

end
```

Compiles to:

```
$ ./assembler jonas-replicator.red -r
0000000000001001
000000000100000000000000000000100000000000000000
000000000000000000000000000000000000000000000000
000000000001010000000000000010010001111111111111
000000000001101100011111111111100000000000000101
000000001001000000011111111111110001111111111101
000000001010100000000000000000110000000000000000
000000000010010000000001101000010000000000000010
000000000100000000011111111110110000000000000000
000000000000010100000000000000000000010100111001
```

With the `--verbose` tag the format can be examined:

```
$ ./assembler jonas-replicator.red --verbose
0000000000001001  00 09  ; Number of rows (9) jonas-replicator.red

   pad    OP  A  B  pad     A op      pad      B op
00000000 0100 00 00 000 0000000000010 000 0000000000000   00 40 00 02 00 00  ; JMP  start           (2)  0(0)
00000000 0000 00 00 000 0000000000000 000 0000000000000   00 00 00 00 00 00  ; DAT  0               (0)  0(0)
00000000 0001 01 00 000 0000000001001 000 1111111111111   00 14 00 09 1f ff  ; MOV # size(9)  src       (-1)
00000000 0001 10 11 000 1111111111110 000 0000000000101   00 1b 1f fe 00 05  ; MOV @ src(-2) < dst      (5)
00000000 1001 00 00 000 1111111111111 000 1111111111101   00 90 1f ff 1f fd  ; DJN  copy(-1)  src(-3)
00000000 1010 10 00 000 0000000000011 000 0000000000000   00 a8 00 03 00 00  ; SPL @ dst            (3)  0(0)
00000000 0010 01 00 000 0000110100001 000 0000000000010   00 24 01 a1 00 02  ; ADD # step(417)  dst      (2)
00000000 0100 00 00 000 1111111111011 000 0000000000000   00 40 1f fb 00 00  ; JMP  start           (-5)  0(0)
00000000 0000 01 01 000 0000000000000 000 0010100111001   00 05 00 00 05 39  ; DAT # 0(0) # init       (1337)
```

If you're interested in the project you can [read more on github][MARC].

In addition, or as a prerequisite, to this awesome project we also got an introduction to 68k assembly and to computer architecture which was very good.

This year me and a friend also made game in the Java programming course called [Grand Thief Arto][]. Not the greatest of games but it was still fun to make it. I also became a mentor in discrete math for the first year students which was a fun and easy way to earn a bit of extra money.

[the microcode]: https://github.com/treeman/MARC/blob/master/scripts/microcode
[VHDL code]: https://github.com/treeman/MARC/blob/master/src/MARC/microcontroller.vhd

[redcode assembler]: https://github.com/treeman/MARC/blob/master/scripts/assembler
[microcode compiler]: https://github.com/treeman/MARC/blob/master/scripts/control_codes

[corestd]: http://corewar.co.uk/icws88.txt "Core Wars 88 standard"


# Third year

This year we continued with some more advanced courses. We had some project oriented courses with some theory about how projects should be run and then a practical part where we made some software for an actual company. The overall idea the course idea is great, to get some real world experience and to get some interaction with an actual company, but the execution was very much not so.

For example in the middle of the project one of our project members were supposed to be exchanged with another member from another project. The motivation was that "in the real world this could happen, so you need to always be prepared" but that's just stupid in my opinion. Firstly you don't get fired and just booted from the team very often, but you get an advanced notice and you have some time to prepare the team, yourself and your eventual replacement for the move. Secondly it adds a very real sense of dread and nervousness for everyone involved. I happened to really like all members in the project and I was very worried to be switched out. Turns out I was the one who had to change project. I was very upset and I didn't click nearly as well with the new team and I thought the experience really sucked and I became demotivated.

Another thing which was quite bad was the extreme focus on documentation. The whole first 2-3 months were supposed to be a planning stage and the actual implementation was supposed to be done the last 2-3 months. But large parts of the implementation stage were also spent on documentation, or rather writing documents because the course demanded it and we never had any use for the majority. The funny thing is that most of the project theory was spent on motivating why the waterfall development model wasn't suited to software development, but what did the project degenerate into? Even despite our best efforts, we tried to implement an iterative development as our customers didn't really know what they wanted, but we were foiled by course planners.

A much better project was the [Robot project][] where we created an autonomous robot! There we poked around with hardware and lower level programming in C.

![Debugging this was quite an experience](/images/trap14/Robot_0018.JPG)

I continued to be a mentor in discrete math and I joined [Team Obelix][] which organized student visits to different companies. I won the "best CV" award given by Ericsson, I won some things in the local programming competition [IMPA][] and I had a great course in operative systems where we used [Pintos][]. I took an extra course in Graph Theory which was quite different from other math courses but very interesting. Additionally I took a [Programming Languages][] course at Coursera, also quite nice.

This summer I also had my first real programming job which was a summer internship at [Configura][] and I had a really good time and we replaced their triangulation software.

[Programming Languages]: https://www.coursera.org/course/proglang
[Configura]: http://www.configura.com/ "Configura"
[Team Obelix]: http://obelix.cyd.liu.se/
[Pintos]: http://web.stanford.edu/class/cs140/projects/pintos/pintos.html


# Fourth year

The courses from the previous years were all mandatory, with the exception of some extra courses I took, but for the 4th and 5th year I could choose my own profile and what courses to take. I spent quite a lot of time thinking about which courses I wanted to take. In the end I'm quite happy with my selection but there are many interesting courses I had to say no to. The main profile I targeted was the [Programming and Algorithms][pal] profile.

In hindsight these are my favourite courses during the year:

1. TDDD48 Automated Planning
1. AAPS Advanced Algorithmic Problem Solving
1. TDDB44 Compiler Construction
1. TATA54 Number Theory
1. TDDC17 Artificial Intelligence
1. TDDD56 Multicore and GPU Computing
1. TDDA69 Data and Program Structures ([SICP][])

I didn't find the compiler course *as* fun as I had initially thought. The theory part wasn't very interesting but the lab part was really, really good! All in all it still was a very fun course. The AI courses (Automated Planning and Artificial Intelligence) were great and AI is an area I'm very interested in. During this time I also took an [online course in Machine Learning][ml-coursera] via Coursera by Stanford University. Number Theory is another maybe niche area in mathematics which I found interesting.

I continued doing some programming competitions as can be seen in [UVa][] which enabled my to go to [NWERC 2013][] in the Netherlands. Ultimately I'm not super good at these competitions but it was still a great experience. Netherlands (or specifically Delft) was great. In the Multicore and GPU Computing course there was a parallel sorting contest which I and my lab partner managed to win. The goal was to create a sorting routine on the CPU with the best sorting performance. Oh and I also went through the classic book Structure and Interpretation of Computer Programs or [SICP][] in a course. Very excellent book!

During the summer I also took part in [IDA Summer of Code 2014][isoc] where I contributed to [rust][]. I also had my [second summer job at Configura][config2]:

![Our octree implementation](/images/configura14/octree2.png)

[config2]: /blog/2014/07/13/summer_job_at_configura/
[isoc]: /blog/2014/10/06/ida_summer_of_code_2014_summary/
[rust]: https://github.com/rust-lang/rust

[UVa]: http://uhunt.felix-halim.net/id/115705
[ml-coursera]: https://www.coursera.org/learn/machine-learning
[pal]: https://www.ida.liu.se/edu/ugrad/program/profiler/pal/index.sv.shtml
[NWERC 2013]: http://2013.nwerc.eu/
[SICP]: https://mitpress.mit.edu/sicp/full-text/book/book.html


# Fifth year

At the end of my fourth year I had grown a bit study tired. I had thought the summer jobs would have helped a bit but although the summer jobs were amazing it might have been better to take a bit of a holiday, which I did not do at all.

But the real issue wasn't me getting sick of studying, although I was a bit unmotivated, but I was fast approaching a burnout. It never got so serious that I became apathetic but it was pretty bad for a while. The last semester in my 4th year I only did 27 hp (30 hp is the target value) and the first semester in my 5th year I only managed 24 hp. I had accumulated extra points so I finished the university with 308.5 hp, of the required 30 hp, but I was less effective the last 1.5 year or so than usual.

I might write more about my burnout in another post, but I did three main things to get out of it. The first thing I did was to start CBT (Cognitive Behavioral Therapy or Kognitiv beteendeterapi in Swedish). The second was to continue with things which made me happy and more importantly gave me more energy. Basically I started exercising more regularly. And the third thing I did was to scale down on my ambitions and focus on taking care of myself, this is the reason I didn't study 100% the first semester.

In the end I think I managed to get rid of the worst effects of the burnout and I managed to finish my studies. I was also a mentor in discrete math for a fourth time. I didn't complete a ton of courses as the whole last semester was spent on writing my [Master's thesis][] but I did complete a great course in cryptology and one about logic programming in Prolog.

The cryptology course was especially good as it even introduced [Bitcoin][] and the way the blockchain worked. Big kudos to that! I was aware of Bitcoin before but I wasn't aware of the implementation details.

[Bitcoin]: https://bitcoin.org/en/ "Bitcoin"
[Master's thesis]: /masters_thesis/ "My Master's thesis"
[Robot project]: /blog/2013/01/20/i_robot/ "Our robot project"
[MARC]: https://github.com/treeman/MARC "Memory Array Redcode Computer"
[Grand Thief Arto]: /blog/2011/10/19/grand_thief_arto/ "Grand Thief Arto"
[uhunt]: http://uhunt.felix-halim.net/id/115705 "Overview of my UVa profile"
[IMPA]: https://www.ida.liu.se/projects/impa/new/results "IDA Mästerskapet i Programmering och Algoritmer"
[saja]: http://linkoping-taekwondo.se/ "ITF Taekwon-do in Linköping"


# All courses

For reference this is the list of courses I completed. It surprises me at least to see how much I've actually done these years.

I started two extra courses which I never finished:

* TAMS15   Mathematical Statistics, first course
* TATA49   Geometry with Applications

The statistics course was started during my first year, but I finished a similar in my later years and the geometry course was taken as a fun extra course but I later decided to skip it.

## First year

* TATA41   Calculus in One Variable 1
* TATA42   Calculus in One Variable 2
* TATA65   Discrete Mathematics
* TDDC10   Perspectives to Computer Technology
* TDDC66   Computer Systems and Programming
* TDDC67   Functional Programming and Lisp
* TDDC68   Imperative Programming and Ada
* TSEA22   Switching Theory and Logical Design
* TSTE58   Electronics
* TTIT02   Foundation Course in Mathematics

## Second year

* TAOP33   Combinatorial Optimization, Introductory Course
* TATA24   Linear Algebra
* TATA53   Linear Algebra, Honours Course
* TATA61   Multivariable and Vector Calculus
* TDDC36   Logic
* TDDC69   Object Oriented Programming and Java
* TDDC70   Data Structures and Algorithms
* TDDD60   Interactive Systems
* TFYY68   Engineering Mechanics
* TSEA43   Digital Project Laboratory
* TSEA47   Computer Hardware and Architecture, part 1
* TSEA49   Computer Hardware and Architecture, part 2

## Third year

* TAMS27   Mathematical Statistics
* TATA50   Transform Theory
* TATA64   Graph Theory
* TDDB68   Concurrent Programming and Operating Systems
* TDDC93   Software Engineering Theory
* TDDD09   Software Engineering Project
* TEIO27   Technology Based Entrepreneurship
* TFYA68   Physics
* TGTU50   Visits to Industry
* TSDT18   Signals and Systems
* TSEA29   Microcomputer, Project Laboratory
* TSKS10   Signals, Information and Communication
* TSRT12   Automatic Control Y

## Fourth year

* TANA09   Numerical Algorithms in Computer Science
* TATA54   Number Theory
* TD1077   Advanced Algorithmic Problem Solving
* TDDA69   Data and Program Structures
* TDDB44   Compiler Construction
* TDDC17   Artificial Intelligence
* TDDD14   Formal Languages and Automata Theory
* TDDD20   Design and Analysis of Algorithms
* TDDD48   Automated Planning
* TDDD56   Multicore and GPU Computing

## Fifth year

* TDDD08   Logic Programming
* TDDD38   Advanced Programming in C++
* TGTU49   History of Technology
* TQDT33   Degree Project - Master's Thesis
* TSIT03   Cryptology

