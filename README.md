# streamly-flatparse

Run [flatparse](https://hackage.haskell.org/package/flatparse) parsers on
[Streamly](https://streamly.composewell.com/) streams of byte arrays.

flatparse parsers are fast, but they read one piece of bytes and cannot
stop to wait for more. Bytes that come from a socket or a file come in
arrays cut anywhere. This library stands between the two, in place of the
parsers of Streamly: it gathers the arrays of a stream until a parser has
what it needs, within a size, and gives the stream that follows what was
parsed.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import FlatParse.Basic qualified as FP
import Streamly.Data.Stream (Stream)
import Streamly.External.FlatParse

-- A decimal number on a line of its own.
line :: FP.Parser () Int
line = FP.anyAsciiDecimalInt <* FP.byteString "\n"

numbers :: Monad m => Stream m (Array Word8) -> Stream m (Either (ParseError ()) Int)
numbers = parseMany 100 line
```

## What it gives

- `parseBreak`: a parser on the start of a stream, tried again as arrays
  come, with the stream that follows what it read. `parse` gives the result
  alone.
- `parseBreakOn`: the same after waiting for a delimiter, the parser run
  once on the bytes up to it.
- `parseMany`: a parser again and again, as a stream of what it finds.
- `parseArray`: a parser on one array that holds the whole input.

Each takes the largest number of bytes the parser may be given.

## What the parsers must be like

- For `parseBreak` and `parseMany`, a failure is taken to mean that the
  input is not all there yet. The parser must therefore fail on every
  proper prefix of what it accepts: it must end on something that tells its
  end, such as a delimiter. A parser that succeeds early, like "some
  digits", would stop at the end of an array with more digits to come. An
  error (`cut`, `err`) stops at once: use one where no further byte can
  help.
- For `parseBreakOn`, the delimiter must not occur inside what is parsed.

## Speed

On the head of an HTTP response of 241 bytes (`cabal bench`):

| | |
|---|---|
| flatparse alone, on the bytes | 0.47 μs |
| `parseBreak`, the head in one array | 0.55 μs |
| `parseBreak`, the head cut in three arrays | 1.18 μs |
| `parseBreakOn` the empty line, in one array | 0.80 μs |

With `parseBreak` the input is parsed again from its start at each array:
cheap for what fits in a few arrays, wasteful for long inputs, which
`parseBreakOn` parses once.
