---
layout: post
title: "Slackware update"
tags: Slackware
---

The last time I (re)installed Slackware [I documented what I did][reinstall]. Somehow I managed to really bork my installation and I decided to go through with a larger reinstallation once more. This is a log of some things I did differently.

# Kernel

Before installing the kernel it's nice to check the GPG signature of the downloaded packages. With the [kernel GPG signature][] we can simply do:

```{.bash}
wget linux-XX.tar.xz
wget linux-XX.tar.sign
unxz linux.tar.xz
gpg --verify *.sign
```

When making oldconfig `yes "" | oldconfig` saves time.

# Slackpkg

I didn't use this tool previously, but it can be used to update the official Slackware packages. Just remember to update the gpg signature as well.

```{.bash}
slackpkg update
slackpkg update gpg
```

I didn't take the plunge to slackware-current just yet, I might do it at a later time when I feel I have a lot of unused time (will I ever?), but it's easy to update selected packages.

```{.bash}
slackpkg upgrade git
```

Will only update git for example.

# Perl

I did some strange things here, the one which worked was simply as root:

```{.bash}
cpan install CPAN
cpan install DateTime
...
```

# Slackbuilds with [sbopkg][]

[Slackbuilds][] works well but can be cumbersome. [sbopkg][] is a great little tool which makes downloading and installing much easier.


# Hakyll and xmonad

This gave me a *lot* of problems. At first I installed xmonad and xmonad-contrib from slackbuilds and I tried to install [Hakyll][] from cabal, but conflicts ensued and they couldn't really work together.

I tried to move cabal out from my home director, but didn't find a very satisfactory solution and in the end I just gave up.

The first thing to do is to install ghc from slackbuilds. Then install the Cabal lib and cabal-install from <https://github.com/haskell/cabal/releases> (they are in the same lib).

Then install a newer version of ghc from their prebuilt binaries at <https://www.haskell.org/ghc>.

There I got the error that `libtinfo.so.5` couldn't be found. This was solved by symlinking to libncurses with `ln -s /lib64/libncurses.so.5 /lib64/libtinfo.so.5`.

Then we can install things with cabal as a regular user:

```{.bash}
cabal install cabal-install
cabal install hscolour
cabal install missingH
cabal install hakyll
cabal install xmonad
cabal install xmonad-contrib
```

If you get the error:

```
pandoc-1.15.0.6 failed during the configure step. The exception was:
user error ('/usr/bin/ghc' exited with an error:
/usr/lib64/ghc-7.8.4/unix-2.7.0.1/libHSunix-2.7.0.1.a(execvpe.o): In function
`pPrPr_disableITimers':
execvpe.c:(.text+0x300): multiple definition of `pPrPr_disableITimers'
/root/.cabal/lib/x86_64-linux-ghc-7.8.4/unix-2.7.1.0/libHSunix-2.7.1.0.a(ghcrts.o):(.text+0x0):
first defined here
```

Try rerunning `cabal install cabal-install` and then try to install hakyll again. This shouldn't happen with an updated ghc though.

# From source

1. fish shell
2. neovim (xclip from slackbuilds)
3. rust (nightly build)
4. dzen2 (edit `config.mk` and use Xinerama, XPM and XFT)

If you get

```
rustc: error while loading shared libraries: librustc_driver-7e44814b.so:
cannot open shared object file: No such file or directory
```

it's because rust installed it in `/usr/local/lib` which is not in the default search path. Can add `/usr/local/lib` to `LD_LIBRARY_PATH` or issue `ldconfig /usr/local/lib` for instant gratification.

# Perl6

This can be done with everything as regular user.

```{.bash}
git clone https://github.com/tadzik/rakudobrew ~/.rakudobrew
```

Add `~/.rakudobrew/bin` to path.  Then we can install package manager panda and VM moar:

```{.bash}
rakudobrew build moar
rakudobrew build-panda
```

Then with panda we can install modules:

```{.bash}
panda install Task::Star
...
```

# Postgres

Install from slackbuilds. Check readme!  After installation, to allow for no password for postgres (useful for pure local) alter `/var/lib/pgsql/9.4/data/pg_hba.conf`

and add the line

```
local   all     postgres    trust
```

before all other configurations.

# Missing

This time I'm having trouble with getting sound working in Skype. To be continued... Maybe.

[reinstall]: /blog/2014/07/05/reinstalling_slackware/ "Reinstalling Slackware"
[kernel GPG signature]: https://www.kernel.org/signature.html "kernel.org signature"
[sbopkg]: http://www.sbopkg.org/ "sbopkg"
[slackbuilds]: http://slackbuilds.org/ "Slackbuilds"
[Hakyll]: https://github.com/jaspervdj/hakyll "Hakyll"

