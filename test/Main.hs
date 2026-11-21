-- | flatparse parsers on streams of arrays cut anywhere.
module Main (main) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Functor.Identity (Identity (..))
import FlatParse.Basic qualified as FP
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.StreamK (StreamK)
import Streamly.Data.StreamK qualified as StreamK
import Streamly.External.ByteString qualified as StreamlyBS
import Streamly.External.FlatParse
import Streamly.Data.Array (Array)
import Data.Word (Word8)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (Positive (..), testProperty, (===))

main :: IO ()
main = defaultMain tests

pieces :: Int -> ByteString -> [ByteString]
pieces size bytes
  | BS.null bytes = []
  | otherwise = let (front, back) = BS.splitAt size bytes in front : pieces size back

-- | Bytes as a stream of arrays of a size.
cut :: Int -> ByteString -> StreamK Identity (Array Word8)
cut size = StreamK.fromStream . Stream.fromList . map StreamlyBS.toArray . pieces size

-- | The result of a parse, with the bytes of the stream given back.
broken :: (Either (ParseError e) a, StreamK Identity (Array Word8)) -> (Either (ParseError e) a, ByteString)
broken (outcome, rest) = (outcome, BS.concat (map StreamlyBS.fromArray (runIdentity (Stream.toList (StreamK.toStream rest)))))

-- | A name, a colon, a decimal number and a line feed. A second colon is
-- an error that no further byte can mend.
entry :: FP.Parser String (ByteString, Int)
entry = do
  name <- FP.byteStringOf (FP.skipSome (FP.skipSatisfyAscii (\c -> c >= 'a' && c <= 'z')))
  FP.word8 58
  FP.optional_ (FP.word8 58 *> FP.err "two colons")
  value <- FP.anyAsciiDecimalInt
  FP.word8 10
  pure (name, value)

-- | Some digits: a parser that succeeds on a prefix of what it accepts.
digits :: FP.Parser () Int
digits = FP.anyAsciiDecimalInt

tests :: TestTree
tests =
  testGroup
    "streamly-flatparse"
    [ testGroup
        "parseArray"
        [ testCase "the whole input in one array" $ do
            parseArray entry (StreamlyBS.toArray "volume:42\nrest") @?= Right ("volume", 42)
            parseArray entry (StreamlyBS.toArray "volume:42") @?= Left Mismatch
            parseArray entry (StreamlyBS.toArray "volume::42\n") @?= Left (ParserError "two colons")
        ]
    , testGroup
        "parseBreak"
        [ testProperty "the result and what follows do not depend on how the bytes are cut" $ \(Positive size) ->
            broken (runIdentity (parseBreak 100 entry (cut size "volume:42\nbalance:7\ntail")))
              === (Right ("volume", 42), "balance:7\ntail")
        , testProperty "a failure gives the whole input back, however it is cut" $ \(Positive size) ->
            broken (runIdentity (parseBreak 100 entry (cut size "Volume:42\n")))
              === (Left Mismatch, "Volume:42\n")
        , testProperty "an error of the parser stops at once, the input given back" $ \(Positive size) ->
            broken (runIdentity (parseBreak 100 entry (cut size "volume::42\nbalance:7\n")))
              === (Left (ParserError "two colons"), "volume::42\nbalance:7\n")
        , testCase "an error stops before the rest of the stream is read" $ do
            let endless = StreamK.fromStream (Stream.fromList (map StreamlyBS.toArray ("a::" : repeat "x")))
            fst (runIdentity (parseBreak 1000 entry endless)) @?= Left (ParserError "two colons")
        , testProperty "the parser is given no more than the limit, exactly" $ \(Positive size) ->
            let input = "volume:42\nmore"
             in ( fst (runIdentity (parseBreak 10 entry (cut size input)))
                , broken (runIdentity (parseBreak 9 entry (cut size input)))
                )
                  === (Right ("volume", 42), (Left (TooLong 9), input))
        , testCase "input that ends before the parser is done" $ do
            broken (runIdentity (parseBreak 100 entry (cut 3 "volume:4"))) @?= (Left Mismatch, "volume:4")
            broken (runIdentity (parseBreak 100 entry (cut 3 ""))) @?= (Left Mismatch, "")
            -- Exactly as many bytes as allowed, and no more to come.
            broken (runIdentity (parseBreak 8 entry (cut 3 "volume:4"))) @?= (Left Mismatch, "volume:4")
        , testCase "endless input that never fits" $ do
            let endless = StreamK.fromStream (Stream.fromList (map StreamlyBS.toArray (repeat "abc")))
            fst (runIdentity (parseBreak 50 entry endless)) @?= Left (TooLong 50)
        , testCase "empty arrays are passed over" $
            broken (runIdentity (parseBreak 100 entry (StreamK.fromStream (Stream.fromList (map StreamlyBS.toArray ["", "vol", "", "ume:4", "", "2\nx"])))))
              @?= (Right ("volume", 42), "x")
        , testCase "a parser that succeeds on a prefix stops too early: what the documentation warns of" $ do
            fst (runIdentity (parseBreak 100 digits (cut 2 "12345 "))) @?= Right 12
            fst (runIdentity (parseBreak 100 digits (cut 100 "12345 "))) @?= Right 12345
        , testCase "parse gives the result alone" $
            runIdentity (parse 100 entry (cut 4 "volume:42\n")) @?= Right ("volume", 42)
        ]
    , testGroup
        "parseBreakOn"
        [ testProperty "the parser runs on the bytes up to the delimiter, however they are cut" $ \(Positive size) ->
            broken (runIdentity (parseBreakOn "\r\n\r\n" 100 (FP.byteStringOf (FP.skipMany FP.anyWord8) :: FP.Parser () ByteString) (cut size "a: b\r\nc: d\r\n\r\nbody")))
              === (Right "a: b\r\nc: d\r\n\r\n", "body")
        , testCase "a parser that succeeds on a prefix is safe here" $
            broken (runIdentity (parseBreakOn " " 100 digits (cut 2 "12345 678"))) @?= (Right 12345, "678")
        , testProperty "no delimiter within the limit, or before the end" $ \(Positive size) ->
            ( broken (runIdentity (parseBreakOn "\n\n" 5 digits (cut size "123456\n\n")))
            , broken (runIdentity (parseBreakOn "\n\n" 100 digits (cut size "123456\n")))
            )
              === ((Left (TooLong 5), "123456\n\n"), (Left Mismatch, "123456\n"))
        , testCase "a delimiter that ends exactly at the limit is found" $
            broken (runIdentity (parseBreakOn "\n\n" 5 digits (cut 2 "123\n\nx"))) @?= (Right 123, "x")
        , testCase "a parser that fails, or errs, on the block gives the input back" $ do
            broken (runIdentity (parseBreakOn "\n" 100 entry (cut 3 "Volume:1\nx"))) @?= (Left Mismatch, "Volume:1\nx")
            broken (runIdentity (parseBreakOn "\n" 100 entry (cut 3 "volume::1\nx"))) @?= (Left (ParserError "two colons"), "volume::1\nx")
        ]
    , testGroup
        "parseMany"
        [ testProperty "every entry of a stream, however it is cut" $ \(Positive size) ->
            let input = BS.concat [BS8.pack ("name:" <> show n <> "\n") | n <- [1 .. 50 :: Int]]
             in runIdentity (Stream.toList (parseMany 100 entry (StreamK.toStream (cut size input))))
                  === [Right ("name", n) | n <- [1 .. 50]]
        , testCase "the first error is the last element" $
            runIdentity (Stream.toList (parseMany 100 entry (StreamK.toStream (cut 4 "a:1\nb::2\nc:3\n"))))
              @?= [Right ("a", 1), Left (ParserError "two colons")]
        , testCase "input cut short ends on a mismatch, and empty input gives nothing" $ do
            runIdentity (Stream.toList (parseMany 100 entry (StreamK.toStream (cut 4 "a:1\nb:")))) @?= [Right ("a", 1), Left Mismatch]
            runIdentity (Stream.toList (parseMany 100 entry (StreamK.toStream (cut 4 "")))) @?= []
        ]
    ]
