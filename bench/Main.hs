-- | What it costs to run a flatparse parser on a stream of arrays, over
-- running it on one piece of bytes: the head of an HTTP response, whole
-- in one array as it mostly comes, and cut in three; and many short lines
-- parsed one after the other, as from a file.
module Main (main) where

import Data.ByteString (ByteString, dropWhileEnd)
import Data.ByteString qualified as BS (drop, dropWhile, null, splitAt, take)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Char8 qualified as BS8 (map)
import Data.Char (chr, isDigit)
import Data.Functor.Identity (Identity (..))
import Data.Word (Word8)
import FlatParse.Basic (Parser, Result (OK), anyAsciiDecimalInt, anyWord8,
                        byteString, byteStringOf, eof, failed, many, runParser,
                        skipMany, skipSatisfyAscii, skipSome, word8, (<|>))
import Streamly.Data.Array (Array)
import Streamly.Data.Fold qualified as F (foldl')
import Streamly.Data.Stream (fold, fromList)
import Streamly.Data.StreamK (StreamK, fromFoldable)
import Streamly.External.ByteString (toArray)
import Streamly.External.FlatParse (ParseError, parseArray, parseBreak,
                                    parseBreakOn, parseMany)
import Test.Tasty.Bench (bench, bgroup, defaultMain, nf)

main :: IO ()
main =
  defaultMain
    [ bgroup
        "HTTP response head, 241 bytes"
        [ bench "flatparse alone, on the bytes" (nf alone httpHead)
        , bench "parseArray, on one array" (nf (either (const Nothing) Just . parseArray httpFlat) whole)
        , bench "parseBreak, the head in one array" (nf (through (parseBreak 65536 httpFlat)) [whole])
        , bench "parseBreak, the head cut in three arrays" (nf (through (parseBreak 65536 httpFlat)) thirds)
        , bench "parseBreakOn the empty line, in one array" (nf (through (parseBreakOn "\r\n\r\n" 65536 httpFlat)) [whole])
        , bench "parseBreakOn the empty line, cut in three arrays" (nf (through (parseBreakOn "\r\n\r\n" 65536 httpFlat)) thirds)
        ]
    , bgroup
        "5000 short lines, 28 kB"
        [ bench "flatparse alone, one parser for all the lines" (nf allLines linesBytes)
        , bench "parseMany, within 64 bytes, one array" (nf (total 64) [toArray linesBytes])
        , bench "parseMany, within 64 bytes, arrays of 4096 bytes" (nf (total 64) (map toArray (pieces 4096 linesBytes)))
        , bench "parseMany, within 64 bytes, arrays of 16 bytes" (nf (total 64) (map toArray (pieces 16 linesBytes)))
        , bench "parseMany, without a limit, one array" (nf (total maxBound) [toArray linesBytes])
        ]
    ]
  where
    allLines bytes = case runParser (sumLines 0) bytes of
      OK found _ -> found
      _ -> -1
    sumLines !sofar = (line >>= \found -> sumLines (sofar + found)) <|> (sofar <$ eof)
    total :: Int -> [Array Word8] -> Int
    total limit arrays = runIdentity (fold (F.foldl' (\sofar -> either (const (-1)) (sofar +)) 0) (parseMany limit line (fromList arrays)))
    alone bytes = case runParser httpFlat bytes of
      OK found _ -> Just found
      _ -> Nothing
    whole = toArray (httpHead <> "the start of a body")
    thirds = map toArray [BS.take 80 httpHead, BS.take 80 (BS.drop 80 httpHead), BS.drop 160 httpHead <> "the start of a body"]
    through :: (StreamK Identity (Array Word8) -> Identity (Either (ParseError ()) a, StreamK Identity (Array Word8))) -> [Array Word8] -> Maybe a
    through driver arrays = either (const Nothing) Just (fst (runIdentity (driver (fromFoldable arrays))))

-- | The head of an image response, as a Roon Server sent it.
httpHead :: ByteString
httpHead =
  "HTTP/1.1 200 OK\r\nAccess-Control-Allow-Origin: *\r\nEtag: 96f45bde867a0aaf6df5ff2f47526aa6916ed3ff:6a8027a650750256c151c53e7ee9017f\r\n\
  \Accept-Ranges: bytes\r\nCache-Control: max-age=21600, public\r\nContent-Type: image/jpeg\r\nContent-Length: 43183\r\n\r\n"

httpFlat :: Parser () (Int, ByteString, [(ByteString, ByteString)])
httpFlat = do
  byteString "HTTP/1."
  skipSatisfyAscii isDigit
  word8 32
  status <- anyAsciiDecimalInt
  reason <- restOfLine
  found <- many headerLine
  lineEnd
  pure (status, reason, found)
  where
    headerLine = do
      name <- byteStringOf (skipSome (skipSatisfyAscii tokenChar))
      word8 58
      value <- restOfLine
      pure (BS8.map lower name, value)
    restOfLine = do
      rest <- byteStringOf (skipMany (anyWord8 >>= \byte -> if byte == 10 then failed else pure ()))
      word8 10
      pure (BS.dropWhile blank (dropWhileEnd blank rest))
    lineEnd = skipMany (word8 13) *> word8 10
    blank byte = byte == 32 || byte == 9 || byte == 13
    tokenChar c = c > ' ' && c < '\DEL' && c /= ':'
    lower c = if c >= 'A' && c <= 'Z' then chr (fromEnum c + 32) else c

-- | Five thousand signed numbers, each on its line.
linesBytes :: ByteString
linesBytes = pack (concat [(if even n then 'R' else 'L') : show (n * 37 `mod` 1000) <> "\n" | n <- [1 .. 5000 :: Int]])

-- | A letter for the sign, a decimal number and a line feed.
line :: Parser () Int
line = do
  sign <- (1 <$ word8 82) <|> ((-1) <$ word8 76)
  found <- anyAsciiDecimalInt
  word8 10
  pure (sign * found)

pieces :: Int -> ByteString -> [ByteString]
pieces size bytes
  | BS.null bytes = []
  | otherwise = let (front, back) = BS.splitAt size bytes in front : pieces size back
