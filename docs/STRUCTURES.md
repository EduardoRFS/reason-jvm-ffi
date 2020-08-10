# Structures

## Why not 1:1

We cannot do 1:1 mapping with a `.class` because in Java everything is in a single namespace so everything is recursive, but making everything in a single OCaml file would be painfully slow to build.

So map every `.class` to a module containing only functors and later in a single file we apply everything

## Why 2 functors?

There is some limitations in the OCaml object system, namely you cannot have mutually recursive modules extending a class, so if X and Y is mutually recursive but Y extends X, that is going to fail at runtime(... yeah).

So you need a functor with `class` only to describe the X -> Y relationship on the object system.
