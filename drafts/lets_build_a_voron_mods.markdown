---
title: "Let's build a VORON: Quality of life mods"
tags: ["3D printing", "VORON"]
series: voron_trident
---

The printer is done, but I feel one of the best part of a VORON printer is the ability to modify and personalize it.
There a *tons* of mods you can do, and while I've done a few there are lots more that I'd like to do some day.

It's a wonderful feeling to augment the printer with parts made by the printer itself.

# RockNRoll

![](/images/trident/rocknroll.jpg)

Flipping over the printer to access the electronics apartment is a huge pain.
There's a very cool [inverted electronics bay mod][] for Trident printers, but it's a tall ask for me to redo _all_ the wiring at this point.

But then I found the simple [RockNRoll][] mod that allows you to easily tilt the printer.
Just having the rockers doesn't work as the center of gravity is too high for the Trident, but with [these additional stilts][rock-stilts] it works great.

# Removable panels

![](/images/trident/magnetic_top.jpg)

Having to screw and unscrew the panels when modding or messing around with the toolhead got old fast.
Therefore I made the panels easily removable by using magnets and snap latches.

The top is completely magnetic using the [Magnetic panels with Magnet Inserts][] mod and is very easy to remove and replace.

![Installation of the corner magnets.](/images/trident/corner_jig.jpg)

I played around with the amazing [snap latches][] mod to hold the panels together:

![The latch closes with a very satisfying thud.](/images/trident/snap_latch.jpg)

But they're a bit cumbersome to lock and unlock, so I compromised and used the latches in the middle of the side panels and used magnets for the corners:

![](/images/trident/magnetic_side.jpg)

I also made the bottom panel magnetic using [the bottom panel mag clip][] mod:

[snap latches]: https://mods.vorondesign.com/detail/9Rdnf5vD2oaJLmR7BpAuQ "Printable snap latches for 2020 extrusion"
[the bottom panel mag clip]: https://mods.vorondesign.com/detail/mRnQfulRJGN3pfPBbSjzA "Bottom Panel Mag Clip"
[Magnetic panels with Magnet Inserts]: https://mods.vorondesign.com/detail/GawFyXN2J0rlSecCAJUpZQ "Magnetic panels with Magnet Inserts"

![](/images/trident/magnetic_bottom.jpg)

Necessary?
No, but they sure are nice.

# Angry cam

![](/images/trident/angrycam_mount.jpg)

The mod I most appreciate is probably having an integrated camera.
It's great to be able to just glance at my phone to see how the print is going, instead of having to go down to the basement to check.

I used the [Angry CAM USB][] mod to mount a small camera module in the front of the printer, above the doors.
The camera itself showed up in Mainsail and to my relief it worked immediately.
To configure resolution you can alter `crowsnest.conf` like so:

```
[cam 1]
mode: ustreamer                         # ustreamer - Provides mjpg and snapshots. (All devices)
resolution: 2592x1944                   # widthxheight format
max_fps: 15                             # If Hardware Supports this it will be forced, otherwise ignored/coerced.
```

To see what resolution and fps the camera supports, you can take a look in `~/printer_data/logs/crowsnest.log`.

I used the MJPEG IP Camera integration to get the feed into Home assistant, using `http://192.168.1.32/webcam/?action=stream` as the URL and made a quick dashboard for the printer:

![Can you smell what the rock is cooking?!](/images/trident/ha_galileo.png){ width=80% }

I should probably rework this and all other dashboards one day...

The mount works well, but the camera is far from perfect.
It's good enough for checking in on the print, but a more clear view would've been nice.
The focus isn't working well, the colors are off and I don't see the whole build plate.
Worse, the toolhead often covers up the print so I can't see if it's still printing well.

I won't do anything about it right now, but in the future I'll probably try to replace it with something else.

[Angry CAM USB]: https://mods.vorondesign.com/detail/RYpQW53mtem8Nj1JKqiSQ>

# Filament runout sensor

<https://www.3djake.com/bigtreetech/smart-filament-sensor-v20>
<https://www.printables.com/model/683859-bigtreetech-smart-filament-sensor-v20-mounting-bra>

# Flex plate stops

::: Flex
/images/trident/flexplate_endstop.jpg
/images/trident/flexplate_endstop2.jpg
:::

Aligning the flex plate isn't too much of a pain, but adding some [flex plate stops][] makes the process easier.

To fit it at the back I removed the now unused z-endstop (I use Tap instead).

[flex plate stops]: https://www.printables.com/model/411428-voron-24-flex-plate-stops>

# Gridfinity mounts

TODO image

I've been slowly easing into [Gridfinity][]---a free and open organization system.
So naturally I wanted to add some some holders to the printer.

I used a combination of [top][top-gridfinity] and [bottom][bottom-gridfinity] holders, and they work great.

# Meshed panels

TODO image

<https://www.printables.com/model/455437-voron-trident-mesh-skirts>

<https://www.printables.com/model/373916-trident-mesh-skirts-ssps-only>

# Galileo 2

![](/images/trident/galileo_6.jpg)

I replaced the Clockwork 2 extruder with the [Galileo 2].

This wasn't exactly needed, I just thought it would be interesting as building the Clockwork was one of the most interesting parts of the build, and this [planetary gears][] thing looked pretty interesting.
And the FOMO kicked in as I was following the discussion online, with people hyping about it but not being able to find a kit.
So when I happened to find one I couldn't help myself.

I was planning to make a separate post only about the build, but the build was surprisingly very quick so I think it works if I include it in this blog post.

![I was worried about the quality of my printed parts, but they fit well.](/images/trident/galileo_1.jpg)

![The planetary gears.](/images/trident/galileo_2.jpg)

![Time to shove the motor into place.](/images/trident/galileo_3.jpg)

![The drive gear is installed.](/images/trident/galileo_5.jpg)

The only difficulty I encountered was to align the drive gear.
It touched the printed part, and I was worried my prints came out all wrong.
The manual says that you can push down the carrier shaft, but I had to disassemble it and realign it before attaching the motor.
After the reassembly it luckily aligned properly.

# Purge bucket

The kit comes with a steel brush and references the [Decontaminator Purge Bucket & Nozzle Scrubber][purge] mod that I was quite excited for.

Unfortunately, Tap decreases the range of motion for the toolhead so that the nozzle no longer can reach back behind the bed:

![This is as far back the nozzle can go. As you can see, it doesn't reach the (now unused) z endstop, and there's no room for a purge bucket / nozzle scrubber.](/images/trident/noise_y_reach.jpg)

There are some things I could do:

- Ditch Tap and use Klicky probe instead.

- Replace the printed Tap with a CNC Tap.

  The Chaoticlab CNC Voron Tap v2 should save around 4mm in the y-axis while Vitalii's version should save around 1.4mm.

- [Sensorless XY homing][sensorless] might also give back some range.

- Moving the bed is a theoretical solution, but that seems difficult to me.

I bought a [Chaoticlab CNC Voron Tap][chaotic_tap], but had I known about the sensorless homing I might have tried that out first.

[sensorless]: https://docs.vorondesign.com/community/howto/clee/sensorless_xy_homing.html "Setting Up and Calibrating Sensorless XY Homing"
[chaotic_tap]: https://www.3djake.com/chaoticlab/cnc-voron-tap-black-v2


[Galileo 2]: https://github.com/JaredC01/Galileo2

[Noctua FN-A6x25]: https://noctua.at/en/nf-a6x25-flx
[4pin]: https://www.nicksherlock.com/2022/01/driving-a-4-pin-computer-pwm-fan-on-the-btt-octopus-using-klipper/
[noctua-pins]: https://faqs.noctua.at/en/support/solutions/articles/101000081757
[nevermore-trident]: https://www.ldomotion.com/p/guide/Nevermore-V5-Duo--Trident
[purge]: https://github.com/VoronDesign/VoronUsers/tree/master/orphaned_mods/printer_mods/edwardyeeks/Decontaminator_Purge_Bucket_%26_Nozzle_Scrubber
[inverted electronics bay mod]: https://github.com/VoronDesign/VoronUsers/tree/master/printer_mods/LoganFraser/TridentInvertedElectronics
[RockNRoll]: https://mods.vorondesign.com/detail/tiIhFDTh9tHJY0JNJK9A
[rock-stilts]: https://www.printables.com/model/638776-voron-rocknroll-mod-stilts/files
[Gridfinity]: https://www.youtube.com/watch?v=ra_9zU-mnl8
[top-gridfinity]: https://www.printables.com/model/175108-gridfinity-holder-for-voron-printers-2020-extrusio
[bottom-gridfinity]: https://www.printables.com/model/431489-gridfinity-mount-for-2020-extrusions-voron-printer
[planetary gears]: https://www.engineeringclicks.com/planetary-gears/
