---
title: "Let's build a VORON: Software"
tags: ["3D printing", "VORON"]
series: voron_trident
---

# Firmware flashing

![The Raspberry Pi and the display are alive and kicking, although the display is rotated 180 degress.](/images/trident/display_alive.jpg)

# Initial config

## Bed

The bed, through the SSR, is connected to `HE0`, corresponding to the `PA2` pin (`BED_OUT` is `PA1`):

```cfg
[heater_bed]
# SSR Pin - HE0
heater_pin: PA2
```

## Hotend

And the hotend is connected to the non-standard `HE1` (since my cable didn't come with a fork spade to connect it to `BED_OUT`, as advised in the [LDO wiring docs][]):

```cfg
[extruder]
# Heater - HE1
heater_pin: PA3
sensor_type: PT1000
```

It also took a long time for me to figure out the correct `sensor_type` value.
None of the values in the [common thermistors][] documentation worked for the Phateus Rapido, but I found an off-hand comment somewhere that `PT1000` was the correct type.

## Tap

Configuring Tap was straightforward, I just followed the steps in the [Updating your Klipper config for Tap][tap_klipper] instructions.

The one thing that confused me was this this line about `PROBE_CALIBRATE`:

> You'll need to manually calibrate the probe's Z offset by using PROBE_CALIBRATE.

Turns out it's the process described in [configuring the Z Offset Adjustment][z_offset] in the VORON documentation, only substituting `Z_ENDSTOP_CALIBRATE` with `PROBE_CALIBRATE`. I ended up with this line in the config:

```cfg
#*# [probe]
#*# z_offset = -1.195
```

Then how good is Tap?

After warming up the enclosure a little bit I ran `PROBE_ACCURACY` to find out:

```
probe accuracy results:
    maximum -1.219375, minimum -1.221250, range 0.001875,
    average -1.220500, median -1.220625, standard deviation 0.000612
```

According to the docs this is a well-built machine:

> For well-built machines you can expect to see between 0.0000 and 0.0008 standard deviation.

That's great!

I'm a little confused what the negative numbers mean; is my `z_offset` too low, causing the probe values to be negative?
Or are they uncorrelated?

## Extruder

Invert direction

I don't understand how you're supposed to properly measure the amount of filament when calibrating
the extruder.
What I did was to pull it taught and try to measure from there, but I fear it wasn't *that* precise.

![The small red mark was 100mm away from the edge before I tried to extrude 100mm of filament. I think this is good enough?](/images/trident/extrude_calibration2.jpg)


```
[extruder]
dir_pin: PF0                     # Invert direction
rotation_distance: 20.41105599   # After recalibration
```

![I hope my first print will look better than this mess?](/images/trident/extrude_calibration.jpg)

# Panel mounting

You may have noticed that the above pictures had panels on them.
That's right, to be able to heat soak the printer I had to have the panels, so I quickly installed them.

![It was easier to align the doors with the printer lying down.](/images/trident/front_mounting.jpg)

![How are you supposed to get the right polarity and stick the part on the correct spot on the door? Just place the magnet and then push in the part.](/images/trident/front_magnet_mount.jpg)

I've been impressed with the build so far, but I'm not impressed with the doors.
The doors doesn't have foam tape between them and the frame unlike the rest of the panels.

I wonder why that is?
I worry that they will scramble against the frame and it won't make a good seal, which I think could've been avoided.

I'll probably look for mods for the doors, maybe I'll replace the doors with a magnetically mounted panel or something.

# Slicer

# First print

# Some pieces missing

![](/images/trident/full_with_panels.jpg)




[tap_klipper]: https://github.com/VoronDesign/Voron-Tap/blob/main/config/tap_klipper_instructions.md
[probe_calibrate]: https://docs.vorondesign.com/build/startup/#z-endstop-location-v0
[z_offset]: https://docs.vorondesign.com/build/startup/#z-offset-adjustment
[common thermistors]: https://www.klipper3d.org/Config_Reference.html#common-thermistors
[LDO wiring docs]: https://docs.ldomotors.com/en/voron/voron-trident/wiring_guide_250_rev_a
