```yaml
[buildPlans.iosevka-tree]
family = "Iosevka Tree"
spacing = "term"
serifs = "sans"
no-ligation = true
no-cv-ss = true
export-glyph-names = false

[buildPlans.iosevka-tree.variants.design]
capital-g = "toothless-rounded-inward-serifed-hooked"
b = "toothless-corner-serifless"
d = "toothless-rounded-serifed"
g = "double-storey"
i = "hooky"
l = "tailed-serifed"
m = "earless-single-arch-short-leg-serifless"
n = "earless-rounded-straight-serifless"
p = "earless-corner-serifless"
q = "earless-corner-straight-serifless"
u = "toothless-corner-serifless"
y = "straight-turn-serifless"
one = "base"
six = "open-contour"
asterisk = "hex-low"
ampersand = "upper-open"
dollar = "open"
percent = "dots"
```

```fish
for x in ls ~/src/Iosevka/dist/iosevka-tree/ttf/iosevka-tree-*; ./font-patcher -c --dry ~/src/Iosevka/dist/iosevka-tree/ttf/iosevka-tree-bold.ttf --out ~/iosevka-tree/; end
```
