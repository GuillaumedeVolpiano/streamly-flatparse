-- |
-- Module      : Streamly.External.FlatParse
-- Description : flatparse parsers on streams of byte arrays
-- License     : BSD-3-Clause
--
-- A flatparse parser is fast, but it reads one piece of bytes and cannot
-- stop to wait for more. Bytes that come from a socket or a file come in
-- arrays cut anywhere. The functions here stand between the two: they
-- gather the arrays of a stream until a parser has what it needs, within a
-- size, and give the stream that follows what was parsed.
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- > import FlatParse.Basic qualified as FP
-- > import Streamly.Data.Stream qualified as Stream
-- > import Streamly.External.FlatParse
-- >
-- > -- A decimal number on a line of its own.
-- > line :: FP.Parser () Int
-- > line = FP.anyAsciiDecimalInt <* FP.byteString "\n"
-- >
-- > numbers :: Monad m => Stream.Stream m (Array Word8) -> Stream.Stream m (Either (ParseError ()) Int)
-- > numbers = parseMany 100 line
--
-- = What the parsers must be like
--
-- Two ways to gather the input, two requirements:
--
-- * 'parseBreak' tries the parser again each time more bytes have come. A
--   failure ('FP.Fail') is taken to mean that the input is not all there
--   yet, so the parser __must fail on every proper prefix of what it
--   accepts__: it must end on something that tells its end, such as a
--   delimiter. A parser that succeeds early, like \"some digits\", would
--   stop at the end of an array with more digits to come. An error
--   ('FP.Err', as 'FP.cut' and 'FP.err' make) stops at once: use one where
--   no further byte can help. The input is parsed again from its start at
--   each attempt, which is cheap for what fits in a few arrays, and
--   wasteful for long inputs.
--
-- * 'parseBreakOn' waits for a delimiter and runs the parser once, on the
--   bytes up to it. The parser need not fail on prefixes, and nothing is
--   parsed twice, but the delimiter must not occur inside what is parsed.
--
-- A flatparse parser cannot be turned into a Streamly @Parser@, to be put
-- together with others: the results here come with the rest of the
-- stream instead, to go on from.
module Streamly.External.FlatParse
  ( -- * Errors
    ParseError (..)

    -- * One piece of input
  , parseArray

    -- * A stream of arrays
  , parseBreak
  , parse
  , parseBreakOn
  , parseMany
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import FlatParse.Basic qualified as FP
import Streamly.Data.Array (Array)
import Streamly.Data.Stream (Stream)
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.StreamK (StreamK)
import Streamly.Data.StreamK qualified as StreamK
import Streamly.External.ByteString qualified as StreamlyBS

-- | Why a parser gave nothing.
data ParseError e
  = -- | The parser failed, and no more input could change that: the
    -- stream had ended.
    Mismatch
  | -- | The parser stopped on an error of its own.
    ParserError e
  | -- | The parser would need more than the bytes allowed: the limit.
    TooLong Int
  deriving stock (Eq, Show)

-- | Run a parser on one array, which holds the whole input. What the
-- parser did not read is left out.
parseArray :: FP.Parser e a -> Array Word8 -> Either (ParseError e) a
parseArray parser array = case FP.runParser parser (StreamlyBS.fromArray array) of
  FP.OK found _ -> Right found
  FP.Fail -> Left Mismatch
  FP.Err problem -> Left (ParserError problem)

-- | Run a parser on the start of a stream of arrays, and give the stream
-- that follows what it read. The parser is given at most a number of
-- bytes; it is tried again as arrays come, so it must fail on every
-- proper prefix of what it accepts (see the top of this module).
--
-- After a failure the stream given back is the input from its start,
-- nothing lost.
parseBreak :: Monad m => Int -> FP.Parser e a -> StreamK m (Array Word8) -> m (Either (ParseError e) a, StreamK m (Array Word8))
parseBreak limit parser = gather BS.empty
  where
    gather buffered input =
      StreamK.uncons input >>= \case
        Nothing -> pure (either id id (attempt True buffered StreamK.nil))
        Just (array, rest)
          | BS.null bytes -> gather buffered rest
          | otherwise -> either (const (gather (buffered <> bytes) rest)) pure (attempt False (buffered <> bytes) rest)
          where
            bytes = StreamlyBS.fromArray array
    -- Try the parser on what has come so far, no further than the limit:
    -- 'Right' when that settles it, 'Left' when more input may help.
    attempt atEnd gathered rest =
      let (allowed, beyond) = BS.splitAt limit gathered
          back leftover = prepend (leftover <> beyond) rest
       in case FP.runParser parser allowed of
            FP.OK found leftover -> Right (Right found, back leftover)
            FP.Err problem -> Right (Left (ParserError problem), back allowed)
            FP.Fail
              | atEnd && BS.null beyond -> Right (Left Mismatch, back allowed)
              -- With exactly as many bytes as allowed, whether the input
              -- is too long shows with the next array, or with the end.
              | not (BS.null beyond) -> Right (Left (TooLong limit), back allowed)
              | otherwise -> Left (Left Mismatch, back allowed)

-- | Like 'parseBreak', without the stream that follows.
parse :: Monad m => Int -> FP.Parser e a -> StreamK m (Array Word8) -> m (Either (ParseError e) a)
parse limit parser input = fst <$> parseBreak limit parser input

-- | Wait for a delimiter in a stream of arrays, run a parser once on the
-- bytes up to its end, delimiter included, and give the stream that
-- follows. The delimiter is looked for in the first bytes of the stream,
-- no further than a number of them.
--
-- After a failure the stream given back is the input from its start.
parseBreakOn :: Monad m => ByteString -> Int -> FP.Parser e a -> StreamK m (Array Word8) -> m (Either (ParseError e) a, StreamK m (Array Word8))
parseBreakOn delimiter limit parser = gather BS.empty
  where
    gather buffered input = case BS.breakSubstring delimiter (BS.take limit buffered) of
      (before, found)
        | not (BS.null delimiter) && not (BS.null found) ->
            let size = BS.length before + BS.length delimiter
                (block, after) = BS.splitAt size buffered
             in pure $ case FP.runParser parser block of
                  FP.OK value _ -> (Right value, prepend after input)
                  FP.Fail -> (Left Mismatch, prepend buffered input)
                  FP.Err problem -> (Left (ParserError problem), prepend buffered input)
        | BS.length buffered >= limit -> pure (Left (TooLong limit), prepend buffered input)
        | otherwise ->
            StreamK.uncons input >>= \case
              Nothing -> pure (Left Mismatch, prepend buffered StreamK.nil)
              Just (array, rest) -> gather (buffered <> StreamlyBS.fromArray array) rest

-- | Run a parser again and again on a stream of arrays, as 'parseBreak'
-- does, and give what it finds. The stream ends with the input, or after
-- the first error, which is its last element.
parseMany :: Monad m => Int -> FP.Parser e a -> Stream m (Array Word8) -> Stream m (Either (ParseError e) a)
parseMany limit parser = Stream.unfoldrM next . Just . StreamK.fromStream
  where
    next Nothing = pure Nothing
    next (Just input) =
      nonEmpty input >>= \case
        Nothing -> pure Nothing
        Just rest ->
          parseBreak limit parser rest >>= \case
            (Right found, following) -> pure (Just (Right found, Just following))
            (Left problem, _) -> pure (Just (Left problem, Nothing))
    -- The stream from its first array that holds something, if any.
    nonEmpty input =
      StreamK.uncons input >>= \case
        Nothing -> pure Nothing
        Just (array, rest)
          | BS.null (StreamlyBS.fromArray array) -> nonEmpty rest
          | otherwise -> pure (Just (StreamK.cons array rest))

-- | A stream with bytes put back before it.
prepend :: ByteString -> StreamK m (Array Word8) -> StreamK m (Array Word8)
prepend bytes stream
  | BS.null bytes = stream
  | otherwise = StreamK.cons (StreamlyBS.toArray bytes) stream
