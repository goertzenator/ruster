# Ruster
A library for creating Erlang NIFs in Rust.  A high-level Rustic API is provided.  Built on [`erlang_nif-sys`](https://github.com/goertzenator/erlang_nif-sys).

## Status

HEAVY CONSTRUCTION

June 2016 update:

Lots of bits are mostly working and have tests, namely...

- Environment and Term abstractions.
- Conversions to and from terms for common types.
- Conversions for Tuples, Binaries, Resources, Atoms.

Docs are really thin, the test application is the best source of examples right now.

This is all still under heavy construction and I will change things if they don't pan
out in practice.  I won't even try to stabalize this until at least Rust macros 2.0 arrives.
