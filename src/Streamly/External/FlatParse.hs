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
parseBreak limit parser input = do
  (outcome, left, rest) <- advance limit parser BS.empty input
  pure (outcome, prepend left rest)
{-# INLINE parseBreak #-}

-- | Run a parser on bytes already taken from a stream of arrays, followed
-- by that stream: the result, the bytes taken that the parser did not
-- read, and the stream after them. Nothing is copied unless the parser
-- needs bytes of a further array: those left over are a slice of those
-- given. After a failure the bytes given back are all those taken.
advance :: Monad m => Int -> FP.Parser e a -> ByteString -> StreamK m (Array Word8) -> m (Either (ParseError e) a, ByteString, StreamK m (Array Word8))
advance limit parser taken input
  | BS.null taken = gather taken input
  | otherwise = attempt False taken input
  where
    -- Try the parser on what has come so far, no further than the limit.
    attempt atEnd gathered rest =
      let allowed = BS.take limit gathered
       in case FP.runParser parser allowed of
            FP.OK found leftover -> pure (Right found, BS.drop (BS.length allowed - BS.length leftover) gathered, rest)
            FP.Err problem -> pure (Left (ParserError problem), gathered, rest)
            FP.Fail
              -- With exactly as many bytes as allowed, whether the input
              -- is too long shows with the next array, or with the end.
              | BS.length gathered > limit -> pure (Left (TooLong limit), gathered, rest)
              | atEnd -> pure (Left Mismatch, gathered, rest)
              | otherwise -> gather gathered rest
    -- More input may help: add the next array that holds something.
    gather gathered rest =
      StreamK.uncons rest >>= \case
        Nothing -> attempt True gathered StreamK.nil
        Just (array, further)
          | BS.null bytes -> gather gathered further
          | otherwise -> attempt False (gathered <> bytes) further
          where
            bytes = StreamlyBS.fromArray array
{-# INLINE advance #-}

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
parseMany limit parser = Stream.unfoldrM next . Just . (,) BS.empty . StreamK.fromStream
  where
    -- The bytes taken from the stream and not parsed yet, and the stream
    -- after them: they are kept as they are from one result to the next,
    -- not put back into the stream.
    next Nothing = pure Nothing
    next (Just (taken, input))
      | BS.null taken =
          nonEmpty input >>= \case
            Nothing -> pure Nothing
            Just (bytes, rest) -> result bytes rest
      | otherwise = result taken input
    result taken input =
      advance limit parser taken input >>= \case
        (Right found, left, rest) -> pure (Just (Right found, Just (left, rest)))
        (Left problem, _, _) -> pure (Just (Left problem, Nothing))
    -- The first array of the stream that holds something, if any.
    nonEmpty input =
      StreamK.uncons input >>= \case
        Nothing -> pure Nothing
        Just (array, rest)
          | BS.null bytes -> nonEmpty rest
          | otherwise -> pure (Just (bytes, rest))
          where
            bytes = StreamlyBS.fromArray array
{-# INLINE parseMany #-}

-- | A stream with bytes put back before it.
prepend :: ByteString -> StreamK m (Array Word8) -> StreamK m (Array Word8)
prepend bytes stream
  | BS.null bytes = stream
  | otherwise = StreamK.cons (StreamlyBS.toArray bytes) stream
