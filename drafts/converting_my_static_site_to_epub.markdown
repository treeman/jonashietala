---
title: "Converting my static site to epub"
tags: Gaming, Ludum Dare
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

And this was after I had corrected my links. Sigh.

Fixing them is annoying and time consuming, but it's not hard.

