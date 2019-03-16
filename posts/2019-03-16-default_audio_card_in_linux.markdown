---
title: "Default audio card in linux"
tags: Void Linux
---

There are a bunch of posts about making your audio work by default in Linux but none that just worked for me.

I had three separate problems:

1. Getting sound
2. Playing sound from multiple sources at the same time
3. Audio card getting different numbers on reboot

# Getting sound

Here I followed [this excellent guide][guide], here's a summary:

Firstly identify card number and device number:

```
~> aplay -l
**** List of PLAYBACK Hardware Devices ****
card 0: Audio [TEAC AI-101 Audio], device 0: USB Audio [USB Audio]
  Subdevices: 0/1
  Subdevice #0: subdevice #0
card 1: Generic_1 [HD-Audio Generic], device 0: ALC1220 Analog [ALC1220 Analog]
  Subdevices: 1/1
  Subdevice #0: subdevice #0
card 1: Generic_1 [HD-Audio Generic], device 1: ALC1220 Digital [ALC1220 Digital]
  Subdevices: 1/1
  Subdevice #0: subdevice #0
card 2: Generic [HD-Audio Generic], device 3: HDMI 0 [HDMI 0]
  Subdevices: 1/1
  Subdevice #0: subdevice #0
card 2: Generic [HD-Audio Generic], device 7: HDMI 1 [HDMI 1]
  Subdevices: 1/1
  Subdevice #0: subdevice #0
card 2: Generic [HD-Audio Generic], device 8: HDMI 2 [HDMI 2]
  Subdevices: 1/1
  Subdevice #0: subdevice #0
card 2: Generic [HD-Audio Generic], device 9: HDMI 3 [HDMI 3]
  Subdevices: 1/1
  Subdevice #0: subdevice #0
card 2: Generic [HD-Audio Generic], device 10: HDMI 4 [HDMI 4]
  Subdevices: 1/1
  Subdevice #0: subdevice #0
card 2: Generic [HD-Audio Generic], device 11: HDMI 5 [HDMI 5]
  Subdevices: 1/1
  Subdevice #0: subdevice #0
```

But to test with `speaker-test` we need info from the `aplay -L` command:

```
~> aplay -L
null
    Discard all samples (playback) or generate zero samples (capture)
sysdefault:CARD=Audio
    TEAC AI-101 Audio, USB Audio
    Default Audio Device
front:CARD=Audio,DEV=0
    TEAC AI-101 Audio, USB Audio
    Front speakers
surround21:CARD=Audio,DEV=0
    TEAC AI-101 Audio, USB Audio
    2.1 Surround output to Front and Subwoofer speakers
surround40:CARD=Audio,DEV=0
    TEAC AI-101 Audio, USB Audio
    4.0 Surround output to Front and Rear speakers
surround41:CARD=Audio,DEV=0
    TEAC AI-101 Audio, USB Audio
    4.1 Surround output to Front, Rear and Subwoofer speakers
surround50:CARD=Audio,DEV=0
    TEAC AI-101 Audio, USB Audio
    5.0 Surround output to Front, Center and Rear speakers
surround51:CARD=Audio,DEV=0
    TEAC AI-101 Audio, USB Audio
    5.1 Surround output to Front, Center, Rear and Subwoofer speakers
surround71:CARD=Audio,DEV=0
    TEAC AI-101 Audio, USB Audio
    7.1 Surround output to Front, Center, Side, Rear and Woofer speakers
iec958:CARD=Audio,DEV=0
    TEAC AI-101 Audio, USB Audio
    IEC958 (S/PDIF) Digital Audio Output
...
```

Now we can test:

```{.bash}
speaker-test -t sin -f 800 -Dfront:Audio -c2        # No sound for me
speaker-test -t sin -f 800 -Dsysdefault:Audio -c2   # This works!
```

Now we know that `card 0: Audio [TEAC AI-101 Audio], device 0` is the card we want. Try this in `~/.asoundrc`:

```
pcm.!default {
  type plug
  slave {
    pcm "hw:0,0"
  }
}
ctl.!default {
  type hw
  card 0
}
```

If we instead have for example `card 1, device 2` then we use `hw:1,2` and `card 1`.

Do `speaker-test -t sin -f 800` to test or something else like youtube.


# Sound from multiple sources

Directly from [the same guide][guide]:

[guide]: http://www.troubleshooters.com/linux/void/voidtips.htm#audio

```
pcm.!default {
    type plug
    slave.pcm "dmixer"
}

pcm.dmixer  {
    type dmix
    ipc_key 1024
    slave {
        pcm "hw:0,0"
        period_time 0
        period_size 1024
        buffer_size 4096
        rate 44100
    }
    bindings {
        0 0
        1 1
    }
}

ctl.dmixer {
    type hw
    card 0
}
```

Make sure to use your card and device numbers.


# Different card numbers after reboot

After reboot the card number changed, it changed between 0, 1 and 2.

There were various suggestions online but most didn't work. I found one suggestion somewhere that said I could just use the card name. I randomly tried something that worked!

```
card 0: Audio [TEAC AI-101 Audio], device 0: USB Audio [USB Audio]
```

The name of the card is "Audio" and the `~/.asoundrc` becomes:

```
pcm.!default {
    type plug
    slave.pcm "dmixer"
}
pcm.dmixer {
    type dmix
    ipc_key 1024
    slave {
        pcm {
            type hw
            card "Audio"
            device 0
        }
        period_time 0
        period_size 1024
        buffer_size 4096
        rate 44100
    }
    bindings {
        0 0
        1 1
    }
}
ctl.dmixer {
    type hw
    card "Audio"
}
```

Much simpler than messing around with udev to get the card to always occupy the same card number, especially since I often turn on and off the amplifier.

