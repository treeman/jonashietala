Personal Weblog
===============

My site <http://www.jonashietala.se>.

Dependencies
============

1. Rust

Sync
====

1. Setup an AWS credentials file or add environment variables. [Docs](https://docs.rs/aws-creds/latest/awscreds/struct.Credentials.html).
2. `./blog sync` to sync `.output` and `./blog upload-files` to upload files not tracked by git.
