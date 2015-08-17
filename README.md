# Ruster
A library for creating Erlang NIFs in Rust.  A high-level Rustic API is provided.  Built on [`ruster_unsafe`](https://github.com/goertzenator/ruster_unsafe).

## Status

HEAVY CONSTRUCTION

Note that the lower level library [`ruster_unsafe`](https://github.com/goertzenator/ruster_unsafe) is in a much more mature state than this project.  If you need to use Rust with Erlang today, use that.

Aug 2015 note: Not much has happened here lately because I was discouraged by my inability to properly protect process independent environments.  But, I've [recently read](http://blog.rust-lang.org/2015/08/14/Next-year.html) that Rust will soon get a more refined borrow-checker that will remove at least some of the road blocks I faced.  I plan on getting on with this project this fall.
