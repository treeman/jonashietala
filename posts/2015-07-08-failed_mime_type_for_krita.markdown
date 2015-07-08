---
layout: post
title: Failed mime type for Krita
tags: Slackware, Krita
---

I thought [Krita][] worked fine... Until I tried to save. Krita crashed whenever I tried to save (or open) a file and gave the error:

```
Could not find mime type "application/x-krita"
```

Turns out if you reinstall the package `shared-mime-info` which comes natively with Slackware the problem goes away.

[Krita]: https://krita.org/ "Krita Digital Painting"
