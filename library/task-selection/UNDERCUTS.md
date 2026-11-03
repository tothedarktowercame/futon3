# Typed undercuts

An accepted rival reading of a pattern's `HOWEVER` clause is recorded in the
attacker's header, after `@references`, as one unwrapped line:

```text
@undercuts task-selection/<target> | shown-by <predicate> | "<verbatim HOWEVER sentence>"
```

The predicate is either `unrecorded` or comparisons over facts present in the
selection record. `other` denotes any other ready candidate in the field when
the line says so explicitly.

Argument X undercuts argument Y when a pattern p of X carries `@undercuts q`, q
is a pattern of Y, and p is not itself a pattern of Y. A cascade containing
both patterns has answered the undercut internally. The attack can defeat only
when its showing predicate holds on Y's pick. An unrecorded or false predicate
is retained as an unshown undercut and defeats nothing.

These four declarations are the rival edges accepted in
`HOWEVER-undercuts-review-2026-11-03.md`; self-warnings and conditions are not
typed as undercuts.
