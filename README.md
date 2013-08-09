# locker - an Erlang per-cluster lock application

[![Build Status](https://travis-ci.org/blt/locker.png)](https://travis-ci.org/blt/locker)

This is a resource locking OTP application for an Erlang cluster. Resources are
identified by a name (`:: term()`) and may have more than one lock taken out for
that resource of any of the provided types. Locks have an associated 'size' and
an optional timeout. By default, locks are of size 3, meaning that three
processes may 'set' the resource lock simultaneously. A process may not set the
same lock more than once. Lock sets are bound to the lifetime of the setting
process: a process crash has no potential to cause a deadlock.

The function `lk:set/1` sets a lock with an indefinite timeout. The function
`lk:set/2`, supplied with the `{timeout, pos_integer()}` option, will set a lock
with a timeout, measured in milliseconds. When the timeout is indefinite a lock
may be unset only by:

  * the setting process calling `lk:del/1` or
  * the setting process crashing.

If the timeout is not indefinite, a lock may be unset by the above and,
additionally, by:

  * the timeout value being eclipsed.

Setting processes are not notified by `locker` if a timeout unsets a lock set by
processes.

## Example

Given two nodes on a host named 'walden' the node `alpha` running the `locker`:

```
> erl -sname alpha -setcookie monster -pa ebin
Erlang R15B03 (erts-5.9.3) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe]
[kernel-poll:false]

Eshell V5.9.3  (abort with ^G)
(alpha@walden)1> net_adm:world_list([walden]).
[beta@walden,alpha@walden]
(alpha@walden)2> application:start(locker).
ok
(alpha@walden)4> application:which_applications().
[{locker,"A lock application for Erlang clusters.","0.0.1"},
 {stdlib,"ERTS  CXC 138 10","1.18.3"},
  {kernel,"ERTS  CXC 138 10","2.15.3"}]
```

and `beta` merely part of the crowd

```
> erl -sname beta -setcookie monster -pa ebin
Erlang R15B03 (erts-5.9.3) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe]
[kernel-poll:false]

Eshell V5.9.3  (abort with ^G)
(beta@walden)1> application:which_applications().
[{stdlib,"ERTS  CXC 138 10","1.18.3"},
 {kernel,"ERTS  CXC 138 10","2.15.3"}]
```

we'll set the magnitude for resource lock `small_lock` down to one

```
(alpha@walden)3> lk:set_size(small_lock, 1).
ok
```

and take the lock on `beta`

```
(beta@walden)2> lk:set(small_lock).
ok
```

but find on `alpha` that the lock resource is overburdened

```
(alpha@walden)5> lk:set(small_lock).
{error,overburdened}
```

Taking pity on `alpha` we'll use `beta` to bump the lock size up

```
(beta@walden)3> lk:set_size(small_lock, 2).
ok
```

and, just as planned, `alpha` will be able to set the lock, which we'll do with
a timeout of 60 seconds

```
(alpha@walden)6> lk:set(small_lock, [{timeout, timer:seconds(60}]).
ok
```

Promptly, we'll start a third node called `gamma`, join it to the cluster and
try to take out `small_lock`

```
> erl -sname gamma -setcookie monster -pa ebin
Erlang R15B03 (erts-5.9.3) [source] [64-bit] [smp:8:8] [async-threads:0] [hipe]
[kernel-poll:false]

Eshell V5.9.3  (abort with ^G)
(gamma@walden)1> net_adm:world_list([walden]).
[alpha@walden,beta@walden,gamma@walden]
(gamma@walden)2> lk:set(small_lock).
{error, overburdened}
```

After a greater than 60 second wait

```
(gamma@walden)3> lk:set(small_lock).
ok
```

## FAQ

### How does this differ from `global:set_lock` and friends?

While this application took much of its initial inspiration from the `global`
module, it differentiates itself by providing:

 * variable 'size' per resource lock,
 * no automatic lock retries,
 * process bound lock sets and
 * expiring lock sets.

Resource lock names are any term, unlike `global` locks which are a bit more
complicated. All locks are implicitly tied to the caller's PID.

### Are the locks volatile?

Yes, absolutely. All information about locks and their holders is stored in
memory on a single node of a cluster. If this node should fail, all the `locker`
information is lost.

The library is intended to be paired with appropriate `kernel` configuration to
keep `locker` running at all times but makes no attempt to do so on its own.

### What are the performance implications of the library?

Fast Enough for my use-case. :)

More seriously though, I haven't written automatic benchmarks for this library
and if you _do_ write such a thing I'd love to see it.

## Future Work

1. `locker` does not have a distributed stress test suite. It should.

## LICENSE

`locker` is provided under the MIT License. Please see `LICENSE` in the root of
this project for details.
