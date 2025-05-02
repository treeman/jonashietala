---
title: MARC
link: http://github.com/treeman/MARC
year: 2012
homepage: true
---

This is a VHDL implementation of the [Core Wars 88 standard][corewars88].
Core Wars is a computer game where two programs compete and try to destroy each other.

The programs may look like this:

```
; This program will copy a DAT instruction to every 4th line in the memory,
; hoping to catch enemy processes and kill them off.
;
; add 4 to the instruction at offset 3 (the DAT)
ADD #4, 3
; copy the instruction at offset 2 to relative location of
; the second field of instruction 2 (+4 lines after DAT the first execution)
MOV 2, @2
; jump back to beginning
JMP -2,0
; if a process tries to execute this line, it dies
DAT #0, #0
```



See [the Core Wars wiki][corewarswiki] for more info.

[corewarswiki]: http://en.wikipedia.org/wiki/Core_War "Core War"
[corewars88]: http://corewar.co.uk/icws88.txt "The Core Wars 88 standard"

