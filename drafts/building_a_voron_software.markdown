---
title: "Let's build a VORON: Software"
tags: ["3D printing", "VORON"]
series: voron_trident
---

The build continues, now in a little more familiar territory.

# Firmware flashing

To flash the firmware I had to order a microSD card reader.
With that on hand the flashing wasn't difficult. The [VORON docs][software installation] walks you through the installation very well.

## Mainsail

I chose to install Mainsail instead of Fluidd simply because I liked the screenshot a little more.
From what I understand they're very similar and the choice doesn't really matter.
I flashed it using [pi-imager].

It was great to see the display turning on being able to ssh to the Pi.

![The Raspberry Pi and the display are alive and kicking, although the display is rotated 180 degress.](/images/trident/display_alive.jpg)


After installing the Octopus firmware on it's SD card and adding the device to `~/printer_data/config/printer.cfg` I could start messing around with the Mainsail web interface.

```
[mcu]
# This is my serial, not yours
serial: /dev/serial/by-id/usb-Klipper_stm32f446xx_140045000F50535556323420-if00
```

![The Mainsail web interface.](/images/trident/mainsail.png)

## KlipperScreen

It's good to see my old friend the Linux login prompt on the display, but that's not really what we want here.
Instead we should install [KlipperScreen] to be able to control the printer via a user-friendly UI.

I installed it using a [manual install][klipper-install], but in hindsight I maybe should've [KIAUH]. Oh well.

To rotate the screen you edit `/boot/config.txt` according to the [LDO docs][rotate-docs]:

```
# Make sure this line is commented
#dtoverlay=vc4-fkms-v3d

# Add these 2 lines to the end
display_lcd_rotate=2
dtoverlay=rpi-ft5406,touchscreen-inverted-x=1,touchscreen-inverted-y=1
```

![The display is rotated and uses KlipperScreen.](/images/trident/klipperscreen.jpg)

## Home assistant

# Initial config

It's not enough to install the firmware, there are quite a few things in `~/printer_data/config/printer.cfg` that needs to be changed.
I used the [LDO config] as a starting point and went from there.
To not bore you to death I'll try to only document the noteworthy changes I made.

And---very importantly---I installed Neovim on the Raspberry Pi I using [snapd].
While I have the configuration files in a git repo, it's far easier to ssh and edit the files directly.

## Be careful

As advised in the [VORON initial startup docs][] when testing these things (motors in particular) you should have a *tested* way of stopping the printer if something goes wrong.
Like if you're ramming the toolhead against the edge or something.

I... Didn't do this and just saw the "Home" button and thought---I wonder if it moves when I press this?

Luckily the XY motors worked as expected (I hadn't configured Z yet) but it could've been very bad.

## Bed

The bed---through the SSR---is connected to `HE0`, corresponding to the `PA2` pin (`BED_OUT` is `PA1`):

```cfg
[heater_bed]
# SSR Pin - HE0
heater_pin: PA2
```

## Hotend

The hotend is connected to the non-standard `HE1` (since my cable didn't come with a fork spade to connect it to `BED_OUT`, as advised in the [LDO wiring docs][]):

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

[According to the docs][tap_docs] the standard deviation falls within the expected range of a well-built machine:

> For well-built machines you can expect to see between 0.0000 and 0.0008 standard deviation.

I doubt that I built everything that well, but I'll take it!

I'm a little confused what the negative numbers mean; is my `z_offset` too low, causing the probe values to be negative?
Or are they uncorrelated?

## Extruder

It's weird.
In some cases you can move mountains and solve the most difficult problems in a flash, but at other times you'll stumble on the smallest pebble.

And I stumbled hard on inserting filament into the extruder.

I know I checked it when assembling the Clockwork 2, but by god I couldn't get it to grab on the filament.
After a long time of fiddling, and considering if I should just disassemble it all, I somehow got it to work.

I think it was a combination of the extruder direction being inverted and some filament getting stuck, but I just don't know.
I'll just file it under user error and forget about it.

```
[extruder]
dir_pin: PF0 # Inverted by removing the `!`
```

I don't understand how you're supposed to properly measure the amount of filament when calibrating
the extruder, the VORON docs isn't particularly clear about it.
What I did was to pull it taught and try to measure from there, but I fear it wasn't *that* precise.

![The small red mark was 100mm away from the edge of the black holder part before I tried to extrude 100mm of filament. I think this is good enough?](/images/trident/extrude_calibration2.jpg)

```
[extruder]
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

# What's left?

The next thing is finally **to print something**.
Unfortunately I don't have any filament other than the small sample that came with the LDO kit, so it's back to waiting for a shipment to be delivered.

And after that there are still things left to do:

1. I'm missing some printed parts that I need to print out myself.

   For example I need a rear exhaust cover (although I plan to add a HEPA filter there) and spacers to mount the handles.

1. Tuning, tuning and more tuning.

   Prints needs to look good.

1. Mod it.

   It feels wrong to build an incredibly moddable printer and leave it without any mods.
   I'm trying not to keep the "to-mod list" short but I can already sense that I'll fail miserably.

![It works, but it's not done yet.](/images/trident/full_with_panels.jpg)

[tap_klipper]: https://github.com/VoronDesign/Voron-Tap/blob/main/config/tap_klipper_instructions.md
[probe_calibrate]: https://docs.vorondesign.com/build/startup/#z-endstop-location-v0
[z_offset]: https://docs.vorondesign.com/build/startup/#z-offset-adjustment
[common thermistors]: https://www.klipper3d.org/Config_Reference.html#common-thermistors
[LDO wiring docs]: https://docs.ldomotors.com/en/voron/voron-trident/wiring_guide_250_rev_a
[software installation]: https://docs.vorondesign.com/build/software/
[pi-imager]: https://www.raspberrypi.com/software/
[KlipperScreen]: https://klipperscreen.readthedocs.io/en/latest/
[klipper-install]: https://github.com/KlipperScreen/KlipperScreen/blob/master/docs/Installation.md#manual-install
[KIAUH]: https://github.com/dw-0/kiauh
[rotate-docs]: https://docs.ldomotors.com/en/guides/btt_43_rotate_guide
[LDO config]: https://github.com/MotorDynamicsLab/LDOVoronTrident/tree/master/Firmware
[snapd]: https://snapcraft.io/install/snapd/raspbian
[VORON initial startup docs]: https://docs.vorondesign.com/build/startup/
[tap_docs]: https://github.com/VoronDesign/Voron-Tap
