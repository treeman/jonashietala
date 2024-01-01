---
title: "Building Git"
tags: Gaming, Ludum Dare
---

# Learned a lot of useful things about gits

- Revisions `git help revisions`

# Some things are more difficult

Some things are just easier in Ruby. Compare this reference code for parsing a Tree:

```ruby
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


```rust
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

# Parsing Revision

```rust
pub enum Revision {
    Ref { name: String },
    Parent { rev: Box<Revision> },
    Ancestor { rev: Box<Revision>, n: u32 },
}

impl Revision {
    pub fn parse(revision: &str) -> Option<Self> {
        if let res @ Some(_) = Self::parse_parent(revision) {
            return res;
        }
        if let res @ Some(_) = Self::parse_ancestor(revision) {
            return res;
        }
        if Self::is_valid_ref(revision) {
            let name = REF_ALIASES.get(revision).unwrap_or(&revision);
            return Some(Revision::Ref {
                name: name.to_string(),
            });
        }

        None
    }

    fn parse_parent(revision: &str) -> Option<Self> {
        let captures = PARENT.captures(revision)?;
        let rev = Revision::parse(captures.get(1)?.as_str())?;
        Some(Revision::Parent { rev: Box::new(rev) })
    }

    fn parse_ancestor(revision: &str) -> Option<Self> {
        let captures = ANCESTOR.captures(revision)?;
        let rev = Revision::parse(captures.get(1)?.as_str())?;
        let n = captures.get(2)?.as_str();
        Some(Revision::Ancestor {
            rev: Box::new(rev),
            n: n.parse()
                .unwrap_or_else(|_| panic!("Should be able to convert {:?} to u32", n)),
        })
    }

    fn is_valid_ref(revision: &str) -> bool {
        !INVALID_NAME.is_match(revision)
    }
}
```

vs

```ruby
class Revision
  Ref = Struct.new(:name)
  Parent = Struct.new(:rev)
  Ancestor = Struct.new(:rev, :n)

  def self.parse(revision)
    if match = PARENT.match(revision)
      rev = parse(match[1])
      rev ? Parent.new(rev) : nil
    elsif match = ANCESTOR.match(revision)
      rev = parse(match[1])
      rev ? Ancestor.new(rev, match[2].to_i) : nil
    elsif valid_ref?(revision)
      name = REF_ALIASES[revision] || revision
      Ref.new(name)
    end
  end

  def self.valid_ref?(revision)
    INVALID_NAME =~ revision ? false : true
  end
end
```
