---
title: "Let's build a VORON: Mods"
tags: ["3D printing", "VORON"]
series: voron_trident
---

I can print, but there are still some parts I've had to print myself to complete the mods included in the LDO kit, and I also wanted to add more mods I've found.

It's a wonderful feeling to augment the printer with parts made by the printer itself.

# Nevermore filter

# HEPA exhaust filter

While I have the Nevermore filter, it doesn't filter Ultra-Fine Particulates.
For that you need a HEPA filter.
Health is good, and as the printer currently sits in my home office finding a good solution was very important for me.

There were a few solutions with combined carbon and HEPA filter, but since I already have the Nevermore I wanted a standalone HEPA filter.
I found the [Voron HEPA Exhaust Filter][] that replaces the stock filter.

[Voron HEPA Exhaust Filter]: https://github.com/jmattingley23/voron-hepa-exhaust-filter

# Missing parts for mods included in the kit

With Nevermore and a HEPA exhaust filter I have a setup that I feel is safe enough to print ABS, so I can finally print out the parts I've been missing to complete the build.

## Display mount

The display mounts I got from the print-it-forward service wasn't compatible with the screen I got.
Now I have a mount that doesn't require tape to hold it together:

<https://mods.vorondesign.com/detail/Go1zR41hCwy0C1qRubOIcQ>

## Handles

I had to have these spacers to mount the handles included in the kit:

TODO image

2 <https://github.com/MotorDynamicsLab/LDOVoron2/blob/main/STLs/handlebar_spacer_x4.stl>

## LED mounts

There were no instructions on how to install the LEDs, so I taped them on top of the included extrusions covers.
Turns out you were supposed to print a bunch of LED mounts to get the LEDs pointing inwards towards the print for better lighting:

TODO image before

TODO image after

36 <https://github.com/VoronDesign/VoronUsers/blob/master/printer_mods/eddie/LED_Bar_Clip/LED_Bar_Clip_Misumi_version2.stl>

## Purge bucket

<https://github.com/MotorDynamicsLab/LDOVoron2/tree/main/STLs/Purge%20Bucket>

<https://github.com/VoronDesign/VoronUsers/tree/master/orphaned_mods/printer_mods/edwardyeeks/Decontaminator_Purge_Bucket_%26_Nozzle_Scrubber>

# RockNRoll

<https://mods.vorondesign.com/detail/tiIhFDTh9tHJY0JNJK9A>

# Panels

<https://mods.vorondesign.com/detail/9Rdnf5vD2oaJLmR7BpAuQ> (snap latches, for non-front?)

<https://mods.vorondesign.com/detail/GawFyXN2J0rlSecCAJUpZQ> (small strips of VHB)

<https://mods.vorondesign.com/detail/xQAsIUzijkbXU2yXrRKdXg> (seems promising, uses VHB tape)

<https://mods.vorondesign.com/detail/tUfp7j8FTQXdJoKGEr0cg> (just top)

<https://www.printables.com/model/523649-sfpmm-single-front-panel-magnetic-mount-solution-f> (Uh, you need to drill...)

# Flex plate stops

<https://www.printables.com/model/411428-voron-24-flex-plate-stops>

# I foresee more mods in this printer's future

The printer is now Officially Done™ and here's a video of it in action:

TODO

But a project like this is never *really* done now is it?
I still have plenty of ideas I'd like to explore with the printer, for example:

1. Attach a camera so I can view the print from my phone.

1. Connect a filament runout sensor to pause the printer if filament runs out.

   Shouldn't this be standard?

1. Replace the Clockwork 2 extruder with the [Galileo 2].

   It's new and shiny!

But that's a story for another time.

[Noctua FN-A6x25]: https://noctua.at/en/nf-a6x25-flx
[4pin]: https://www.nicksherlock.com/2022/01/driving-a-4-pin-computer-pwm-fan-on-the-btt-octopus-using-klipper/
[noctua-pins]: https://faqs.noctua.at/en/support/solutions/articles/101000081757
[Galileo 2]: https://github.com/JaredC01/Galileo2
