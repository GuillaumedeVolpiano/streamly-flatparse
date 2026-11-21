-- | What it costs to run a flatparse parser on a stream of arrays, over
-- running it on one piece of bytes: the head of an HTTP response, whole
-- in one array as it mostly comes, and cut in three.
module Main (main) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (chr, isDigit)
import Data.Functor.Identity (Identity (..))
import Data.Word (Word8)
import FlatParse.Basic qualified as FP
import Streamly.Data.Array (Array)
import Streamly.Data.StreamK qualified as StreamK
import Streamly.External.ByteString qualified as StreamlyBS
import Streamly.External.FlatParse
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
    ]
  where
    alone bytes = case FP.runParser httpFlat bytes of
      FP.OK found _ -> Just found
      _ -> Nothing
    whole = StreamlyBS.toArray (httpHead <> "the start of a body")
    thirds = map StreamlyBS.toArray [BS.take 80 httpHead, BS.take 80 (BS.drop 80 httpHead), BS.drop 160 httpHead <> "the start of a body"]
    through :: (StreamK.StreamK Identity (Array Word8) -> Identity (Either (ParseError ()) a, StreamK.StreamK Identity (Array Word8))) -> [Array Word8] -> Maybe a
    through driver arrays = either (const Nothing) Just (fst (runIdentity (driver (StreamK.fromFoldable arrays))))

-- | The head of an image response, as a Roon Server sent it.
httpHead :: ByteString
httpHead =
  "HTTP/1.1 200 OK\r\nAccess-Control-Allow-Origin: *\r\nEtag: 96f45bde867a0aaf6df5ff2f47526aa6916ed3ff:6a8027a650750256c151c53e7ee9017f\r\n\
  \Accept-Ranges: bytes\r\nCache-Control: max-age=21600, public\r\nContent-Type: image/jpeg\r\nContent-Length: 43183\r\n\r\n"

httpFlat :: FP.Parser () (Int, ByteString, [(ByteString, ByteString)])
httpFlat = do
  FP.byteString "HTTP/1."
  FP.skipSatisfyAscii isDigit
  FP.word8 32
  status <- FP.anyAsciiDecimalInt
  reason <- restOfLine
  found <- FP.many headerLine
  lineEnd
  pure (status, reason, found)
  where
    headerLine = do
      name <- FP.byteStringOf (FP.skipSome (FP.skipSatisfyAscii tokenChar))
      FP.word8 58
      value <- restOfLine
      pure (BS8.map lower name, value)
    restOfLine = do
      line <- FP.byteStringOf (FP.skipMany (FP.anyWord8 >>= \byte -> if byte == 10 then FP.failed else pure ()))
      FP.word8 10
      pure (BS.dropWhile blank (BS.dropWhileEnd blank line))
    lineEnd = FP.skipMany (FP.word8 13) *> FP.word8 10
    blank byte = byte == 32 || byte == 9 || byte == 13
    tokenChar c = c > ' ' && c < '\DEL' && c /= ':'
    lower c = if c >= 'A' && c <= 'Z' then chr (fromEnum c + 32) else c
