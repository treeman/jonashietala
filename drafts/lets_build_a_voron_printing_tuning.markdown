---
title: "Let's build a VORON: Printing & Tuning"
tags: ["3D printing", "VORON"]
series: voron_trident
---

It's up and running, and now it's finally printing time!
Less exciting---but necessary---tuning the printer to make the prints pretty.

# Filament shipment

Even though I've been building the printer for more than a month, I wasn't ready for it being time for printing so soon.
I don't know what filament is good and what I need, so I decided to get a few different brands and colors to try out:

- [3DJAKE niceABS Black]
- [eSUN ABS+ Purple]
- Polymaker PolyTerra PLA [Army Dark Green] and [Army Blue]
- [3DJAKE ecoPLA White & Black]
- [Spectrum PLA Dragon Red]
- [eSUN eSilk PLA Gold]

![I think I went overboard with the filament order.](/images/trident/filament.jpg)

# First print

The VORON docs [makes the first print seem so simple][voron-print]---just upload the print and eat a bowl of cereal.
And I was looking forward to just hitting print and watching it go brr...
But of course things wouldn't go so smoothly.


Problems I've had include:

1. Print stopped with `Hotend not enough` (Tap reduces temp to 150)

   This happens because the Tap G-code reduces the hotend temperature to 150° when probing to not damage the bed, but then the temperature is too low for printing.

   This made me implement [a better print_start macro][].

1. Filament didn't load.

   And KlipperScreen complains about `FILAMENT_LOAD` not existing. Sigh.

1. First layer not sticking to the bed.

1. After a few failed attempts, the filament clogs.

   ![The filament clogged somehow, and I had to disassemble the toolhead to fix it.](/images/trident/clogged.jpg)

1. Bunch of "Unknown command" errors in the mainsail log.

   I assume it's because Klipper wasn't selected in the "G-code flavor" in SuperSlicer.

https://reprap.org/wiki/G-code

  Unknown command:"M101"
  Unknown command:"M126"
  Unknown command:"M127"


# Secondary printer tuning

Continuing with the VORON docs the next part is the [Secondary printer tuning][].

## Gantry racking & squaring

I've already done the gantry racking, yay!

## Belt tension

I always wondered how tight the belts were supposed to be.
Tuning it by measuring the frequency using an app was pretty nifty, and it was quite painless.

![It's around 110Hz, I think?](/images/trident/belt_frequencies.png)

## Bed mesh

```
[bed_mesh]
speed: 300
mesh_min: 40, 40
mesh_max: 210, 210
fade_start: 0.6
fade_end: 10.0
probe_count: 5,5
algorithm: bicubic
```

And then `BED_MESH_CALIBRATE` will do it's job:

![The bed mesh is situated around 0, seems good.](/images/trident/bed_mesh.png)

I also wanted to use [Klipper Adaptive Meshing & Purging][] (KAMP).

## Input shaping

The guide references input shaping, which is included in the LDO kit.
But [NERO 3D][] recommended to wait with input shaping until you've printed with it a bit, so I've held off on it until I've done some more printing.

# Print tuning

<https://ellis3dp.com/Print-Tuning-Guide/>

# Print comparison

# A functional print

Exhaust cover
  https://github.com/MotorDynamicsLab/LDOVoron2/blob/main/STLs/exhaust_cover.stl

[3DJAKE niceABS Black]: https://www.3djake.com/3djake/niceabs-black?sai=3802
[eSUN ABS+ Purple]: https://www.3djake.com/esun/abs-purple-2?sai=11812
[PLA Army Blue]: https://www.3djake.com/polymaker/polyterra-pla-army-blue?sai=14897
[PLA Army Dark Green]: https://www.3djake.com/polymaker/polyterra-pla-army-dark-green?sai=11933
[3DJAKE ecoPLA White & Black]: https://www.3djake.com/3djake/ecopla-white-black-economy-set?sai=5063
[Spectrum PLA Dragon Red]: https://www.3djake.com/spectrum/pla-dragon-red?sai=3969
[eSUN eSilk PLA Gold]: https://www.3djake.com/esun/esilk-pla-gold?sai=11857
[NERO 3D]: https://www.youtube.com/@Nero3D
[Klipper Adaptive Meshing & Purging]: https://github.com/kyleisah/Klipper-Adaptive-Meshing-Purging
[Secondary printer tuning]: https://docs.vorondesign.com/tuning/secondary_printer_tuning.html
[voron-print]: https://docs.vorondesign.com/build/slicer/first_print.html
[a better print_start macro]: https://github.com/jontek2/A-better-print_start-macro
