---
title: "Let's build a VORON: Noise"
tags: ["3D printing", "VORON"]
series: voron_trident
---

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

The connection looks like this:

![The fans are connected to the PCB splitter using low-noise adapters.  
It's important to move the jumper for the PCB FAN connector.](/images/trident/noctua_wiring.jpg)

I altered the extension cables that came with the kit because it leaves the fans intact and I can control the speed using the included low-noise adapters (I used the ultra-low-noise adapters).
I didn't have a compatible 3-pin splitter for the fans so I used the PCB I had on hand.

I also took the opportunity to cleanup the wiring and place the fans on the other side, closer to the Raspberry Pi:

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


# Loud stepper noise

Another really annoying noise was the steppers.

The first problem was them being noisy while idle.
This was solved by adding stealthchop:

```
stealthchop_treshold: 999999
```

But it was still really loud when moving, especially the xy steppers were super annoying.

This seems to be a [common issue with some 0.9° steppers][09_issue] and I tried manage this with various config settings:

1. Lower `run_current` from 0.8 to 0.6 for x- and y-stepper motors.

   This drastically lowered noise during movement.
   It's not silent and it's still too loud, but it really helped.

   Some have suggested that raising `run_current` might help as well, but for me that just made things worse.

1. Increased microsteps.

   `microsteps: 128` on z an `microsteps: 256` on xy

   As long as the MCU can handle it there should be no downsides, and it does help with lowering noise.

1. I tried `interpolate: true`, but I didn't notice any improvements.

While things aren't *that* bad anymore, while I was tweaking I gave in and ordered two `Wantai 42BYGHM810` steppers from Aliexpress that [according to the linked discussion][09_issue] should be much quieter. (1.8° should generally be much better too.)


# Nevermore fans

https://github.com/nevermore3d/Nevermore_Micro/issues/29


[Noctua FN-A6x25]: https://noctua.at/en/nf-a6x25-flx
[4pin]: https://www.nicksherlock.com/2022/01/driving-a-4-pin-computer-pwm-fan-on-the-btt-octopus-using-klipper/
[noctua-pins]: https://faqs.noctua.at/en/support/solutions/articles/101000081757
[09_issue]: https://klipper.discourse.group/t/stepper-0-9-deg-very-noise/6961/1
