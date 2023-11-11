---
title: "Let's build a VORON: Wiring"
tags: ["3D printing", "VORON"]
series: voron_trident
---

> Dread it. Run from it. Wiring arrives all the same.
{ :epigraph }

It's time for the part of my VORON build that I've been dreading: the wiring.

It's scary because I really have no clue what I'm doing, and mistakes can be very costly and time-consuming.

I've taken my time to double- and triple-check everything, and I took lots of pictures.
I ran into some issues, but this post will mostly be filled with pictures to gawp at.

# Cable chains

There wasn't a ton of instructions on the cable chains, but I felt it went well.

> I made some big mistakes here that caused major headache later on, but [more on that later][chains_short].
{ :notice }

![It was fiddly, but not the end of the world.](/images/trident/chain3.jpg)

::: Flex
/images/trident/chain1.jpg
/images/trident/chain2.jpg
:::

![](/images/trident/loose_top.jpg)
![](/images/trident/chain_pretty.jpg)

::: Flex
/images/trident/hidden_side.jpg
/images/trident/hidden_side_chain.jpg
:::

![If we make the wires pretty from the start, we can just continue like that?](/images/trident/top_motor_cables.jpg)

# Main power

Yes, I guess I should've connected main power before connecting the motor cables, but the octopus isn't connected yet so it's fine...

![I triple-checked that I don't mix colors and I checked again that I connected it to the outlet properly.](/images/trident/mains.jpg)

Note that it says "TO 5V PSU" on the PSU cable. I think that's a typo...

![The green light turns on! I'm glad it didn't burn up yet...](/images/trident/mains_light.jpg)

# Octopus power

According to the docs, the SSR should connect to `HE0` instead of `BED_OUT`. Fine.

![The Octopus has the required power, but I didn't check if it turns on at this point...](/images/trident/octo_power.jpg)

![I mixed the 3 and 4 inputs on the SSR, but luckily I caught it when double or triple-checking.](/images/trident/octo_power2.jpg)

# Breakout cables

I'm following LDO's wiring guide and the breakout cables are next.

::: Flex
/images/trident/breakout1.jpg
/images/trident/breakout2.jpg
:::

## Wrong hotend cable?

When I was about to connect the hotend cable I noticed a problem.

In the [wiring guide][] it says that I should connect the hotend to `BED_OUT`:

> | Breakout PCB 	| Cable label (breakout) 	    | Controller 	| Cable label (controller)
> | :-----------  | :-------------------------    | :---------    | :---------------------------
> | HE0 **** 	    | HE0 	                        | HE0/PA2 	    | BED_OUT
>
> ****The BED_OUT port in the Octopus controller carries more current than the HE ports, this allows you to use super high power hotends such as the Phaetus Rapido.

But the cable that I got doesn't fit:

![The hotend cable has "pin" type connectors.](/images/trident/wrong_hotend_cable.jpg)

![What I want is "U" type connectors.  
(I later found them on Amazon as "fork spade" terminals.)](/images/trident/right_cable_connector.jpg)

My suspicion is that I got the wrong cable because the [V2.4 wiring guide][] specifies that the cable goes into `HE0` and `SSR` goes to `BED_OUT`, while the Trident has it reversed.

I complained about this to 3DJake (where I bought the kit from), who reached out to LDO where I got the response that they're sorry, but I can connect the hotend to `HE1` and change something in `printer.cfg` to make it work.

Not ideal is I do have the Phaetus Rapido, and according to LDO's [V2.4 errata][] it's advisable to switch:

> It has been reported that the Rapido hotend (by Phaetus) draws a large amount of current during initial heat-up. If you are using this hotend with our kit, please consider swapping the bed and hotend connections at the Octopus side.

I guess I need to find some spade terminals and change the wires later.

## Full picture

![A better overview of the wiring with skirts and all.](/images/trident/breakout.jpg)

# Raspberry

The Raspberry Pi uses 5V via some PCBs:

![The Raspberry Pi is connected.](/images/trident/raspberry.jpg)

# Other small details

The major things are done, but there are more things that needs to be fixed.

## LEDs

I got a cute little PCB for connecting two LED strips in the kit.

![It's neat. I like it.  
(I later moved the LED wire so it goes down in the front instead of behind the belts.)](/images/trident/led_pcb.jpg)

## Controller fan

I should've gotten another PCB mount to mount the fan PCB, but I didn't get one from the print-it-forward service.
Maybe I should've mounted the fan PCB with the mount and wait with the LEDs, but I couldn't be bothered.

![Some tape to prevent shorts.](/images/trident/fan_pcb.jpg)

![It was hell to get it mounted. Why didn't I do this before adding wires...?](/images/trident/fan_pcb2.jpg)

![The wire is stuck between the fan and the plastic part, oops.](/images/trident/remove_wire_first.jpg)

## Display mount

I didn't manage to get the display mount to work properly.
The screws didn't align properly and the display cover constantly fell off.

![There's a space here that I couldn't close no matter how hard I pushed.](/images/trident/mount2.jpg)

![The screw hole is slightly off.](/images/trident/mount_bad.jpg)

I had to use tape to keep it all together:

![Tape for the cover so it doesn't fall off.](/images/trident/mount_bad3.jpg)

![Tape for the sides.](/images/trident/mount_bad4.jpg)

It works I guess but I need to print new mounts when I get the printer up and running.
I believe the problem is that I have the 2.1 version of the display, while the mount is for the 2.0 version.

## The bed

I had delayed installing the bed, but with the wiring needing to be done it was time to install the bed.

I was a bit worried about applying the magnetic sheet, but I think I managed to do it without any bubbles.

![The bed is installed and ready.](/images/trident/bed_wired.jpg)

Maybe the Z-endstop is completely unnecessary as I'm going to run Tap?
Probably, but as it's already installed I won't bother ripping it out now.

## Missing Toolhead cables

When [assembling the stealthburner][] I couldn't connect the hotend to the toolhead PCB because the connectors didn't match, and I also didn't have a cable from the Tap PCB to the toolhead PCB.
It was time to rectify that.

I didn't do it when building the toolhead because I didn't have the tools for it.
So I ordered wire strippers, a crimping tool, JST connectors and some wire from Amazon.

No, I didn't have a wire stripper and I've never created a cable before.

![Crimping was fiddly but after a dozen attempts I think I got it down.](/images/trident/crimp.jpg)

After I painstakingly created the Tap cable, with a 3-pin JST on each end, I noticed that the Tap PCB required a smaller connector than the ones I had. Oh no, do I have to order *another* connector kit?.

But luckily the Tap kit came with one such connector.

So I tried to change it... But after trying for some time I noticed that the wires I had were too thick, and didn't fit this smaller connector. 
Things aren't going my way, maybe I need to order (and wait for) new wire?

But wait!

Remember how I complained about the hotend cables having the wrong connector?
Turns out the hotend comes with extension cables with the connectors I wanted, and one of them has the same JST connector and has a thinner wire.
Maybe I could shorten that cable and use the leftover wire for the Tap cable?

![A shortened extension cable for the hotend thermistor.](/images/trident/short_cable.jpg)

![Behold! My glorious cable!](/images/trident/own_cable.jpg)

I've spent an embarrassing amount of time and energy just to create this single cable.
It makes me *really* appreciate that the LDO kit comes with pre-made wiring,
I can't imagine the frustration if I had to create all wiring from scratch (there's a *lot* of it).

One problem I still have is the excessive wiring coming out from the toolhead, and I don't really know what to do with it.

![Excessive wiring. Should I try to hide it inside the cable chain?](/images/trident/cable_bundle2.jpg)

Maybe I could try to shorten them all... But I'm not skilled or brave enough to try.

![It's not pretty... It sort of ruins the nice looking toolhead don't you think?](/images/trident/cable_bundle.jpg)

# Gantry racking

When building the printer I've been jumping around a little, and somewhere in the middle of the wiring I decided I should try to [solve the gantry racking][].

I'd noticed that the gantry catches a little when moving it around, and I got a tip from the [VORON forum][] that I should rack the gantry to try to fix it.

And it did solve the issue!
The movement isn't as smooth as in NERO 3D's video, but at least it doesn't catch anywhere.

> I did this with the motors and everything connected, which is NOT recommended as it may damage the components.
> At one point I saw the display flashing and thought "huh, that's weird", but clueless as I am the implications didn't register at that time.
> 
> Yeah I know that NERO 3D said so in the gantry racking video, but it had slipped my mind.
> Now I'm really worried that I've screwed myself over in a major way.
{ :notice }


# Lack of range for the toolhead

After installing the bed and racking the gantry, I noticed a big issue with the toolhead: it doesn't reach the corners of the bed and the bed doesn't reach up to the toolhead.

![The toolhead is at max x, but it's far from the edge.](/images/trident/short_x.jpg)

![The toolhead doesn't reach the edge on the y-axis either.](/images/trident/short_y.jpg)

![I tried to raise the bed as high as possible, but it doesn't come close to the toolhead, let alone raising it for tap to function.](/images/trident/short_z.jpg)

Turns out I had made a mistake when installing the cable chains, as they're all too short and they max out too soon, stopping the movement.

There were 4 extra links in the kit, but I didn't know what to do with them so I forgot about them and just assumed that the three cable chains would work as-is.
Maybe this is assumed knowledge, but when installing them I should've checked the range of motion to be sure they were long enough.

Now I had to break open the chains and add the extra links.
This was super annoying because I had to pull more wire to the chains, meaning I had to undo all the wiring work for all the motors and toolhead cables.

![Opening up the xy cable chains.](/images/trident/redo_chain.jpg)

I added two links to the z chain and one link each to the x and y chains.
For x and y I also had to add some extra space by offsetting them so the chains aren't flush to the edge of the extrusion holder or toolhead.

![I had to add some extra spacing on the x and y chains to get the required range of motion.
I wish I didn't clip away the tab, so I could zip tie the cables to it.](/images/trident/redo_chain2.jpg)

::: Flex
/images/trident/redo_chain3.jpg
/images/trident/redo_chain4.jpg
:::

# It's all coming together

![All the wiring is in place.  
(You may notice an unconnected cable in the upper right, it's for the Nevermore filter I haven't built yet).](/images/trident/full_wiring.jpg)

I may clean it up a bit after I've verified that things work.
It may not be [r/cableporn][] neat, but it could be worse.

![A better overview of the wiring with skirts and all.](/images/trident/nice_wires2.jpg)

# Let there be light

With everything prepared I closed my hands, curled my toes, and clenched my ass and turned on the power...

And the lights are glowing! *Huzzah!*

![The lights are glowing, and there's no smoke from the Octopus.  
I disconnected the hotend and Raspberry as a safety measure before turning it on.](/images/trident/octopus_lights.jpg)

![The Raspberry Pi also has some lights when turned on.
I think that's a good sign, but I can't tell for sure before flashing.](/images/trident/pi_lights.jpg)

The status lights are promising, but I can't tell for sure before flashing.

[wiring guide]: https://docs.ldomotors.com/en/voron/voron-trident/wiring_guide_250_rev_a
[V2.4 wiring guide]: https://docs.ldomotors.com/en/voron/voron2/wiring_guide_rev_c
[V2.4 errata]: https://docs.ldomotors.com/en/voron/voron2/kit-errata
[assembling the stealthburner]: /blog/2023/10/18/lets_build_a_voron_toolhead/#stealthburner-assembly
[chains_short]: #cable-chains-are-too-short
[solve the gantry racking]: https://www.youtube.com/watch?v=cOn6u9kXvy0
[VORON forum]: https://forum.vorondesign.com/
[r/cableporn]: https://www.reddit.com/r/cableporn/
