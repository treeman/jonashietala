---
title: "Let's build a VORON: Mods"
tags: ["3D printing", "VORON"]
series: voron_trident
---

I can print, but there are still some parts I've had to print myself to complete the build, and I also want to add some mods.

It's a wonderful feeling to augment the printer with parts made by the printer itself.

# Silent controller fans

The very first mod I made (even before my first print) was to replace the LOUD skirt fans with silent [Noctua FN-A6x25].
When I say the fans were loud, I mean that they were **incredibly damn loud**.
I don't see how anyone could be near the printer with fans this loud for a prolonged period of time.

Maybe I'm just sensitive, but I digress.

[You can run PWM fans][4pin], but I didn't see the point so I went with the simple 3-pin variant.
The fans will fit as they are and I only made some small tweaks:

1. Used the 12V selection jumper for the fan output on the Octopus.
1. Converted the 3-pin cable to a 2-pin JST connector, and used the existing fan PCB to split the Octopus output.

   I simply ignored [the yellow RPM speed signal cable][noctua-pins].

The connection looked like this:

![The fans are connected to the PCB splitter using low-noise adapters.](/images/trident/noctua_wiring.jpg)

I altered the extension cables that came with the kit because it leaves the fans intact and I can control the speed using the included low-noise adapters (I used the ultra-low-noise adapters).
I didn't have a compatible 3-pin splitter for the fans so I used the PCB I had on hand.

I also took the opportunity to cleanup the wiring and place the fans on the other side, closer to the Pi:

![The wiring looks pretty neat. I hope I don't have to mess with it in a long time.](/images/trident/clean_wiring.jpg)

You could do something smart with the fan management, but it's now silent enough that I can leave it on all the time.
This is how I set that up in `printer.cfg`:

```
[fan_generic controller_fan]
## Controller fan - FAN2
pin: PD12
kick_start_time: 0.5
max_power: 1.0

# Set the controller fan to be on from startup.
# Speed is controlled by noctua low-noise adapter.
[delayed_gcode controller_fan_boot]
initial_duration: 1.0
gcode:
    SET_FAN_SPEED FAN=controller_fan SPEED=1.0
```

# HEPA exhaust filter

While I have the Nevermore filter, it doesn't filter Ultra-Fine Particulates.
For that you need a HEPA filter.
Health is good, and as the printer currently sits in my home office finding a good solution was very important for me.

There were a few solutions with combined carbon and HEPA filter, but since I already have the Nevermore I wanted a standalone HEPA filter.
I found the [Voron HEPA Exhaust Filter][] that replaces the stock filter.

[Voron HEPA Exhaust Filter]: https://github.com/jmattingley23/voron-hepa-exhaust-filter

# Missing parts for mods included in the kit

I now, finally, have a setup I feel comfortable with printing ABS in the background so I can print out the parts I've been missing to complete the build.

## Display mount

https://mods.vorondesign.com/detail/Go1zR41hCwy0C1qRubOIcQ

## Handles

2 https://github.com/MotorDynamicsLab/LDOVoron2/blob/main/STLs/handlebar_spacer_x4.stl

## LED mounts

36 https://github.com/VoronDesign/VoronUsers/blob/master/printer_mods/eddie/LED_Bar_Clip/LED_Bar_Clip_Misumi_version2.stl

## Purge bucket

https://github.com/MotorDynamicsLab/LDOVoron2/tree/main/STLs/Purge%20Bucket https://github.com/VoronDesign/VoronUsers/tree/master/orphaned_mods/printer_mods/edwardyeeks/Decontaminator_Purge_Bucket_%26_Nozzle_Scrubber

# RockNRoll

https://mods.vorondesign.com/detail/tiIhFDTh9tHJY0JNJK9A

# Panels

https://mods.vorondesign.com/detail/GawFyXN2J0rlSecCAJUpZQ
https://mods.vorondesign.com/detail/xQAsIUzijkbXU2yXrRKdXg
https://mods.vorondesign.com/detail/tUfp7j8FTQXdJoKGEr0cg

# Flex plate stops

https://www.printables.com/model/411428-voron-24-flex-plate-stops

# I foresee more mods in this printer's future



1. A camera
1. Filament runout sensor
1. [Galileo 2]

[Noctua FN-A6x25]: https://noctua.at/en/nf-a6x25-flx
[4pin]: https://www.nicksherlock.com/2022/01/driving-a-4-pin-computer-pwm-fan-on-the-btt-octopus-using-klipper/
[noctua-pins]: https://faqs.noctua.at/en/support/solutions/articles/101000081757
[Galileo 2]: https://github.com/JaredC01/Galileo2
