# bigerror 🍅🍆🥒

Errors should be composed of reusable parts to make readability consistent.

## example

What if handling an error also produces an error?


code:

https://github.com/knox-networks/bigerror/blob/51686e4e42397f4275335a54286210e705ab8ea7/src/lib.rs#L822-L838

command (try it yourself):
```sh
$ cargo test -- test::error_in_error_handling --nocapture
```

output:

<img width="913" alt="image" src="https://github.com/knox-networks/bigerror/assets/11223234/bed07095-64ad-4fc4-bd5d-c770f6c4b19c">

## extra

```sql
-- This error crate is intended to
-- enhance error-stack:
-- https://hash.dev/blog/announcing-error-stack
-- error-stack `is_fantastic && does_things.in(|_a| functional(approach))` but
-- `error_stack::Report::change_context` can make code noisy when used often
--
-- ...so why not auto implement a bunch of `From<Error> for MyError`?
-- and provide fundamental building blocks such as `bigerror::NotFound`
-- to express common causes for errors and imply correlation?
CREATE CRATE IF NOT EXISTS
  bigerror (
    error BIGERROR NOT NULL,
);
```
