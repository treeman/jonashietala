---
layout: post
title: Hearthstone on Wine
tags: Wine, Slackware
---

I like [Hearthstone][] and recently the next expansion [Goblins vs Gnomes][] so I wanted to install and play it. It didn't work in vanilla wine, with a "time out error", but I found a [bug report][] which makes it work. Here's a short summary:

Get wine source from [git][]. My version was `wine-1.7.30-121-g6fe4d9e`.

Make `shlexec.patch` inside the source directory

```{.diff}
@@ -, +, @@ 
    Revert "shell32: Use CREATE_NEW_CONSOLE when SEE_MASK_NOCONSOLE is omitted."
    
    This reverts commit 2005be6dc92c0943ede01525cecad88f8e83c9c7.
--- a/dlls/shell32/shlexec.c	
+++ a/dlls/shell32/shlexec.c	
@@ -338,7 +338,7 @@ static UINT_PTR SHELL_ExecuteW(const WCHAR *lpCmd, WCHAR *env, BOOL shWait,
     startup.dwFlags = STARTF_USESHOWWINDOW;
     startup.wShowWindow = psei->nShow;
     dwCreationFlags = CREATE_UNICODE_ENVIRONMENT;
-    if (!(psei->fMask & SEE_MASK_NO_CONSOLE))
+    if (psei->fMask & SEE_MASK_NO_CONSOLE)
         dwCreationFlags |= CREATE_NEW_CONSOLE;
     if (CreateProcessW(NULL, (LPWSTR)lpCmd, NULL, NULL, FALSE, dwCreationFlags, env,
                        lpDirectory, &startup, &info))
```

Before `make` run

```{.bash}
patch -p1 < shlexec.patch
```

Then do `make` and `make install` as normal and have fun with Hearthstone.

[git]: http://wiki.winehq.org/GitWine "Git Wine"
[bug report]: https://bugs.winehq.org/show_bug.cgi?id=36216 "Hearthstone fails to start, says the game timed out"
[Hearthstone]: http://us.battle.net/hearthstone/en/ "Hearthstone"
[Goblins vs Gnomes]: www.goblinsvsgnomes.com "Goblins vs Gnomes"
