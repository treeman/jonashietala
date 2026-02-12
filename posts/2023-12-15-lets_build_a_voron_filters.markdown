---
title: "Let's build a VORON: Filters"
tags: ["3D printing", "VORON Trident"]
series: voron_trident
---

I can print, but the printer is missing a very important thing that I alluded to in the previous post: filtering dangerous particles and fumes.

This is mostly covered by the kit, but I was missing some parts from the [print it forward][pif] and the kit didn't include a HEPA filter.
Because the printer is in the office and health is super important fixing this was needed so I could start printing some ABS and continue modding the printer.

# Exhaust cover

![I'm missing the exhaust cover where the filter goes in a stock VORON.](/images/trident/exhaust_cover_hole.jpg)

The standard VORON comes with a filter in the back, but the LDO kit doesn't include the fan for the filter and instead uses an [exhaust cover][] to close the hole.

I didn't receive this part, so the first thing I did was to print that part.
This was just a very temporary measure so I printed it in PLA.

# Nevermore filter

![The Nevermore filter is installed next to the back panel.](/images/trident/nevermore_installed.jpg)

The LDO kit includes the [Nevermore filter][], which removes VOCs.
I was confused on how to install it as at the time the LDO docs only mentioned how to install it on a 2.4, not on a Trident.
When I brought it up they [fixed it ][ldo-nevermore] so no big deal.

But they wanted me to mount it using a plenum mount, which my parts didn't have.
I think I received an older version of the Nevermore from the [print it forward][pif] service so I couldn't use it.

I instead printed out the updated plenum and frame connector myself.
Because the point of the Nevermore is to be able to print ABS safely I did this in PLA as well.
I know it's not ideal, but it will have to do.
Lazy as I am I'll run with this and if it breaks I'll replace it then.

I also added some strips of foam tape to the back of the filter to prevent it from scrambling against the back panel.

![Cutting away parts from the fans was horrible. They broke very easily.](/images/trident/nevermore_fans.jpg)

![I finally got to exercise my (very poor) soldering skills!
It was cute how LDO included a small board so that you don't have to crimp any cables.](/images/trident/nevermore_solder.jpg)

# HEPA exhaust filter

![A new HEPA filter. I wasn't able to color match with the print-it-forward parts...](/images/trident/hepa_filter.jpg)

While I have the Nevermore filter, it doesn't filter Ultra-Fine Particulates.
For that you need a HEPA filter.

There were a few solutions with combined carbon and HEPA filter, but since I already have the Nevermore I wanted a standalone HEPA filter.
I found the [Voron HEPA Exhaust Filter][] that replaces the stock filter that seemed like a good option.

With the Nevermore in place I could print the parts using ABS, and they turned out pretty well.

For silence I used a [Noctua FN-A6x25][] fan.
In hindsight I maybe should've used the [PWM variant][4pin], if only to make the wiring simpler.
Now I have this ugly looking wiring in the top corner (it uses the Noctua low-noise adapter).

::: Flex
/images/trident/hepa_filter_wire.jpg
/images/trident/ugly_wire.jpg
:::

I also had to create another cable, which wasn't *too* difficult:

1. Converted the 3-pin cable to a 2-pin JST connector and extended it.

   I simply ignored [the yellow RPM speed signal cable][noctua-pins].

1. Connect to a free fan output on the Octopus.
1. Used the 12V selection jumper for the fan output.

# Klipper setup

With the fans installed you also need to configure Klipper to utilize them.

I setup them both as generic fans, so I can control them from the start and end macros:

```
[fan_generic nevermore_fan]
##  Nevermore fan - FAN3
pin: PD13
max_power: 1.0
kick_start_time: 5.0

[fan_generic filter_fan]
##  HEPA filter fan - FAN4
pin: PD14
max_power: 1.0
kick_start_time: 5.0
```

Then in `PRINT_START` I turn on the fans:

```
SET_FAN_SPEED FAN=nevermore_fan SPEED=1
SET_FAN_SPEED FAN=filter_fan SPEED=1
```

After the print has finished I don't want to just turn off the fans immediately, but have them run a bit after the print has finished to clear out any toxic fumes.
I accomplished this with a `delayed_gcode`:

```
[delayed_gcode _VENT_OFF]
gcode:
  SET_DISPLAY_TEXT MSG="Venting done"
  SET_FAN_SPEED FAN=nevermore_fan SPEED=0
  SET_FAN_SPEED FAN=filter_fan SPEED=0
```

That I call in `PRINT_END` with a timeout:

```
# Turn off fans after 30 min
UPDATE_DELAYED_GCODE ID=_VENT_OFF DURATION=1800
```

To prevent the fans from turning off mid-print if I get impatient and start a new print before `_VENT_OFF` has been called I also clear the delayed gcode in `PRINT_START`:

```
# Prevent Nevermore and filter fan from being turned off mid print
UPDATE_DELAYED_GCODE ID=_VENT_OFF DURATION=0
```

And now the filter fans turn on and off automatically.

Some people run the Nevermore on 50--80% during print and then go 100% at the end, but I don't know why you can't run it at 100% all the time so that's what I do.

## Reworked `PRINT_START`/`PRINT_END`

I also rewrote the `PRINT_START` and `PRINT_END` macros to make a little more sense to me.
Here they are in their entirety:

```
[gcode_macro PRINT_START]
gcode:
  # Fetch data from slicer
  {% set target_bed = params.BED|int %}
  {% set target_extruder = params.EXTRUDER|int %}
  {% set target_chamber = params.CHAMBER|default(0)|int %}

  # Set temps for bed and extruder without waiting to save some time
  SET_DISPLAY_TEXT MSG="Preheating"
  SET_HEATER_TEMPERATURE HEATER=heater_bed TARGET={target_bed}
  SET_HEATER_TEMPERATURE HEATER=extruder TARGET=150

  SET_DISPLAY_TEXT MSG="Homing"
  G28                                                     # Full home (XYZ)
  G90                                                     # Absolute position
  G92 E0                                                  # Reset extruder

  BED_MESH_CLEAR                                          # Clears old saved bed mesh (if any)
  CLEAR_PAUSE                                             # Ensure that we can't accidentally resume an old pause
  # Prevent Nevermore and filter fan from being turned off mid print
  UPDATE_DELAYED_GCODE ID=_VENT_OFF DURATION=0

  SET_DISPLAY_TEXT MSG="Bed: {target_bed}c"
  # We're in the center of the bed after full home
  M190 S{target_bed}                                      # Sets the target temp for the bed

  # Heating nozzle to 150 degrees. This helps with getting a correct Z-home
  SET_DISPLAY_TEXT MSG="Hotend: 150c"
  M109 S150                                               # Heats the nozzle to 150c

  # Turn on fans to help with chamber heating
  SET_FAN_SPEED FAN=nevermore_fan SPEED=1
  M106 S255                                               # Turns on the PT-fan

  # Waits for chamber to reach desired temp
  SET_DISPLAY_TEXT MSG="Heatsoak: {target_chamber}c"
  TEMPERATURE_WAIT SENSOR="temperature_sensor chamber_temp" MINIMUM={target_chamber}

  # Only turn on filter after chamber has been heated
  SET_FAN_SPEED FAN=filter_fan SPEED=1

  SET_DISPLAY_TEXT MSG="Z-tilt adjust"
  Z_TILT_ADJUST                                           # Levels the buildplate via z_tilt_adjust
  G28 Z                                                   # Homes Z again after z_tilt_adjust

  SET_DISPLAY_TEXT MSG="Bed mesh"
  BED_MESH_CALIBRATE                                      # Starts bed mesh

  # Heats up the nozzle up to target via data from slicer
  SET_DISPLAY_TEXT MSG="Hotend: {target_extruder}c"
  SMART_PARK                                              # KAMP parking routine
  M107                                                    # Turns off partcooling fan
  M109 S{target_extruder}                                 # Heats the nozzle to printing temp

  SET_DISPLAY_TEXT MSG="Purge"
  VORON_PURGE                                             # KAMP purge
  G92 E0                                                  # Reset extruder
  SET_DISPLAY_TEXT MSG="Printer goes brr"

[gcode_macro PRINT_END]
gcode:
  # safe anti-stringing move coords
  {% set th = printer.toolhead %}
  {% set x_safe = th.position.x + 20 * (1 if th.axis_maximum.x - th.position.x > 20 else -1) %}
  {% set y_safe = th.position.y + 20 * (1 if th.axis_maximum.y - th.position.y > 20 else -1) %}
  {% set z_safe = [th.position.z + 2, th.axis_maximum.z]|min %}

  SAVE_GCODE_STATE NAME=STATE_PRINT_END

  SET_DISPLAY_TEXT MSG="Print done"

  M400                                                        # Wait for buffer to clear
  G92 E0                                                      # Zero the extruder
  G1 E-2.0 F3600                                              # Retract filament

  TURN_OFF_HEATERS

  G90                                                         # Absolute positioning
  G0 X{x_safe} Y{y_safe} Z{z_safe} F20000                     # Move nozzle to remove stringing
  G0 X{th.axis_maximum.x//2} Y{th.axis_maximum.y - 2} F3600   # Park nozzle at rear
  M107                                                        # Turn off partcooling fan

  # Turn off fans after 30 min
  UPDATE_DELAYED_GCODE ID=_VENT_OFF DURATION=1800

  BED_MESH_CLEAR
  RESTORE_GCODE_STATE NAME=STATE_PRINT_END

[delayed_gcode _VENT_OFF]
gcode:
  SET_DISPLAY_TEXT MSG="Venting done"
  SET_FAN_SPEED FAN=nevermore_fan SPEED=0
  SET_FAN_SPEED FAN=filter_fan SPEED=0
```

[Voron HEPA Exhaust Filter]: https://github.com/jmattingley23/voron-hepa-exhaust-filter
[pif]: https://pif.voron.dev/ "VORON print it forward"
[exhaust cover]: https://github.com/MotorDynamicsLab/LDOVoron2/blob/main/STLs/exhaust_cover.stl "LDO exhaust cover"
[ldo-nevermore]: https://www.ldomotion.com/p/guide/Nevermore-V5-Duo--Trident "LDO Nevermore Trident installation"
[Nevermore filter]: https://github.com/nevermore3d/Nevermore_Micro "Nevermore Micro filter"
[Noctua FN-A6x25]: https://noctua.at/en/nf-a6x25-flx "Noctua FN-A6x25"
[4pin]: https://www.nicksherlock.com/2022/01/driving-a-4-pin-computer-pwm-fan-on-the-btt-octopus-using-klipper/ "Driving a 4-pin computer PWM fan on the BTT Octopus using Klipper"
[noctua-pins]: https://faqs.noctua.at/en/support/solutions/articles/101000081757 "What pin configuration do Noctua fans use?"
