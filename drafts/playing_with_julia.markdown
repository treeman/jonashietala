---
layout: post
title: Playing with Julia
tags: Julia
---

Interactive:

```
Pkg.add("Gadfly")
Pkg.add("Cairo")
using Gadfly
```

```
plot(x=collect(1:100), y=sort(rand(100)), Geom.line)
```

Draw in browser.


