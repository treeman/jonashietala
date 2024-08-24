# Personal Weblog

My site <http://www.jonashietala.se>.

# Sync

1. Setup an AWS credentials file or add environment variables. [Docs](https://docs.rs/aws-creds/latest/awscreds/struct.Credentials.html).

   By default it will use `~/.aws/credentials` if exists.

2. `./blog sync` to sync `.output` and `./blog upload-files` to upload files not tracked by git.

# Syntax highlighting

I use sublime text highlighters. Syntect comes with several built-in, but you can add more manually to the `syntaxes` folder.

Either drop in a `.sublime-syntax` file or add a git submodule, for example:

```bash
cd syntaxes
git submodule add https://github.com/evandroforks/Toml
```

Then update the syntax binary with `./blog dump-syntax-binary`.
