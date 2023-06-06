# About

Fork-join in a simple Common Lisp library, for [fork-join style
concurrency](https://en.wikipedia.org/wiki/Fork%E2%80%93join_model).

It allows one to aggregate concurrent computations ('tines') into groups
('forks') that can be manipulated as a whole. The simplest usage is
```
 (FORK
   (FOO)
   (BAR X)
   (BAZ))

```
that runs `(FOO)`, `(BAR X)` and `(BAZ)` concurrently and waits until all of them complete.
