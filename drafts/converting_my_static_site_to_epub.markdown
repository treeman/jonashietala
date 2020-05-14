---
title: "Converting my static site to epub"
tags: Cryptocurrency, Programming, Pollen, Racket, Why cryptocurrencies?
---

# Validating

The commonly recommended [idpf validator][idpf] is down. I instead used [epubcheck][] I found on github. Turns out I had a few errors:

[idpf]: http://validator.idpf.org/
[epubcheck]: https://github.com/w3c/epubcheck

```
$ java -jar epubcheck.jar ~/code/why_cryptocurrencies/_ebook/why_cryptocurrencies.epub
Check finished with errors
Messages: 2 fatals / 2183 errors / 2 warnings / 0 infos
```

And this was after I had made a bunch of corrections. Sigh.

Fixing them is annoying and time consuming, but it's not hard. Most of them were easily fixed by updating the templates and tag generation within Pollen.

1. Disallowed tags
2. Alt text for images are required
3. Must provide .png fallbacks for all images
4. Cannot use `<a name="...">`
