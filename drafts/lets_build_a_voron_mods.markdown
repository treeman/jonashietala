---
title: "Let's build a VORON: Mods"
tags: ["3D printing", "VORON"]
series: voron_trident
---

- Smaller fixes
- Display mount
- LED mounts
- Spool holder

- Quality of life mods
- Magnetic panels
- RockNRoll
- Handles
- Purge bucket (and CNC Tap)
- Angry cam
- Filament runout sensor

- Gratuitous mods
- Galileo 2
- Meshes ?
- Flex plate stops ?

I can print, but there are still some parts I've had to print myself to complete the mods included in the LDO kit, and I also wanted to add more mods I've found.

It's a wonderful feeling to augment the printer with parts made by the printer itself.

# Mods included in the kit

With Nevermore and a HEPA exhaust filter I have a setup that I feel is safe enough to print ABS, so I can finally print out the parts I've been missing to complete the build.

## Display mount

The display mounts I got from the print-it-forward service wasn't compatible with the screen I got.
Now I have a mount that doesn't require tape to hold it together:

<https://mods.vorondesign.com/detail/Go1zR41hCwy0C1qRubOIcQ>

## Handles

I had to have these spacers to mount the handles included in the kit:

TODO image

2 <https://github.com/MotorDynamicsLab/LDOVoron2/blob/main/STLs/handlebar_spacer_x4.stl>

The kit came with some very sturdy handles, but unfortunately they install over the top panel so you can't remove the panel without removing the handles.
This clashes badly with my plan to make a more easily removable top panel, but fortunately I found 3D printed handles that work.

<https://mods.vorondesign.com/detail/EAM1ZiQJCUzXznvOA767w>

## LED mounts

There were no instructions on how to install the LEDs, so I taped them on top of the included extrusions covers.
Turns out you were supposed to print a bunch of LED mounts to get the LEDs pointing inwards towards the print for better lighting:

TODO image before

TODO image after

36 <https://github.com/VoronDesign/VoronUsers/blob/master/printer_mods/eddie/LED_Bar_Clip/LED_Bar_Clip_Misumi_version2.stl>

## Purge bucket

The kit comes with a steel brush and references the [Decontaminator Purge Bucket & Nozzle Scrubber][purge] mod that I was quite excited for.

Unfortunately, Tap decreases the range of motion for the toolhead so that the nozzle no longer can reach back behind the bed:

![This is as far back the nozzle can go. As you can see, it doesn't reach the (now unused) z endstop, and there's no room for a purge bucket / nozzle scrubber.](/images/trident/noise_y_reach.jpg)

One solution is to replace printed Tap with a [CNC Tap][] which is thinner than the printed Tap I use, so it doesn't compromise range in the Y-direction.

[CNC Tap]: https://www.3djake.com/chaoticlab/cnc-voron-tap-black-v2

# RockNRoll

![](/images/trident/rocknroll.jpg)

Flipping over the printer to access the electronics apartment is a huge pain.
There's a very cool [inverted electronics bay mod][] for Trident printers, but it's a tall ask for me to redo _all_ the wiring at this point.

But then I found the simple [RockNRoll][] mod that allows you to easily tilt the printer.
Just having the rockers doesn't work as the center of gravity is too high for the Trident, but with [these additional stilts][rock-stilts] it works great.

# Bowden Tube Guide

<https://mods.vorondesign.com/detail/8CxQeqS1lXhlGphwkyqh7g>

# Meshed skirts

<https://www.printables.com/model/455437-voron-trident-mesh-skirts>

<https://www.printables.com/model/373916-trident-mesh-skirts-ssps-only>

# Panels

<https://mods.vorondesign.com/detail/9Rdnf5vD2oaJLmR7BpAuQ> (snap latches, for non-front...? Maybe)

<https://mods.vorondesign.com/detail/xQAsIUzijkbXU2yXrRKdXg> (best so far)

<https://mods.vorondesign.com/detail/GawFyXN2J0rlSecCAJUpZQ> (small strips of VHB, uses more magnets and more moving parts)

# Flex plate stops

<https://www.printables.com/model/411428-voron-24-flex-plate-stops>

# Angry cam

<https://mods.vorondesign.com/detail/RYpQW53mtem8Nj1JKqiSQ>

# Filament runout sensor

<https://mods.vorondesign.com/detail/6QtRuihC2dy6oBljKYymw>

# Spool holder

Issue: too short for some spools.

Possible solutions:

- https://mods.vorondesign.com/detail/wWS3pc510oGqxGo0awsFKA just longer
- https://mods.vorondesign.com/detail/VjlccbeeOuH5iax4AFHA top, but needs more things
- https://mods.vorondesign.com/detail/x2umK6ZcG6l2c5EEM2LQjQ can mount horizontally
- https://www.printables.com/model/369877-better-voron-spool-holder
- https://www.printables.com/model/227260-voron-24-top-mount-single-dual-spool-holder dual

# Galileo 2

Replace the Clockwork 2 extruder with the [Galileo 2].

[Noctua FN-A6x25]: https://noctua.at/en/nf-a6x25-flx
[4pin]: https://www.nicksherlock.com/2022/01/driving-a-4-pin-computer-pwm-fan-on-the-btt-octopus-using-klipper/
[noctua-pins]: https://faqs.noctua.at/en/support/solutions/articles/101000081757
[Galileo 2]: https://github.com/JaredC01/Galileo2
[nevermore-trident]: https://www.ldomotion.com/p/guide/Nevermore-V5-Duo--Trident
[purge]: https://github.com/VoronDesign/VoronUsers/tree/master/orphaned_mods/printer_mods/edwardyeeks/Decontaminator_Purge_Bucket_%26_Nozzle_Scrubber
[inverted electronics bay mod]: https://github.com/VoronDesign/VoronUsers/tree/master/printer_mods/LoganFraser/TridentInvertedElectronics
[RockNRoll]: https://mods.vorondesign.com/detail/tiIhFDTh9tHJY0JNJK9A
[rock-stilts]: https://www.printables.com/model/638776-voron-rocknroll-mod-stilts/files
