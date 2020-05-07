---
title: "Building Git"
tags: Gaming, Ludum Dare
---

Some things are just easier in Ruby. Compare this reference code for parsing a Tree:

```{.ruby}
def self.parse(scanner)
  entries = {}

  until scanner.eos?
    mode = scanner.scan_until(/ /).strip.to_i(8)
    name = scanner.scan_until(/\0/)[0..-2]

    oid = scanner.peek(20).unpack("H40").first
    scanner.pos += 20

    entries[name] = Entry.new(oid, mode)
  end

  Tree.new(entries)
end
```

Which is very nice as it uses an unpacking function that magically handles it all for us. I didn't find one in rust, so I did it the hard way:


```{.rust}
pub fn from_scanner(oid: &Oid, mut scanner: Scanner) -> Result<Self> {
    let mut entries = BTreeMap::new();

    while !scanner.is_empty() {
        let mode = util::parse_mode_string(scanner.scan_upto(32))?;
        scanner.advance(1);

        let name = PathBuf::from(String::from_utf8(scanner.scan_upto(0))?);
        scanner.advance(1);

        let oid = Oid::try_unpack(&scanner.take(20)).unwrap();

        let e = if mode == TREE_MODE {
            TreeEntry::Tree(TreeRef::new(name.clone(), oid))
        } else {
            let executable = util::executable_mode(mode);
            TreeEntry::File(FileRef::new(name.clone(), oid, executable))
        };

        entries.insert(name.into_os_string(), e);
    }

    let cell_oid = OnceCell::new();
    cell_oid.set(oid.clone()).unwrap();

    Ok(Tree {
        entries,
        oid: cell_oid,
    })
}
```

