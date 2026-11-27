# Revision history for streamly-flatparse

## 0.1.0.2 -- 2026-11-27

* Builds with GHC 9.8 and 9.10 as well as 9.12: the language is GHC2021
  with the extensions GHC2024 adds, and the bounds on `base` and on the
  libraries that come with the compiler are wider.

## 0.1.0.1 -- 2026-11-21

* `parseMany` no longer copies what follows each result: on an input much
  longer than the limit, it copied the whole rest of the array once for
  each result. It keeps the bytes it has taken from one result to the
  next, and `parseBreak` gives back a slice of them. Thirteen times
  faster on five thousand lines in one array.

## 0.1.0.0 -- 2026-11-21

* `parseBreak`, `parse`, `parseBreakOn`, `parseMany` and `parseArray`:
  flatparse parsers on streams of byte arrays, within a size.
