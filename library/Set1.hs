{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Set1 where

import Data.Word (Word8)
import qualified Data.ByteString        as BSStrict
import qualified Data.ByteString.Lazy.Char8 as Ch8
import qualified Data.ByteString.Lazy        as BS
import qualified Data.ByteString.Base16.Lazy as BS16
import qualified Data.ByteString.Base64.Lazy as BS64
import Data.Bits ((.|.), xor)
import qualified Text.Regex.PCRE as Rx
import qualified Text.Trifecta as Tri
import Text.Trifecta (letter, space, natural, some, parseByteString)
import Data.Decimal (DecimalRaw(..))
import Data.Monoid (Product(..))
import Data.List (find)

data NGram = Mono BS.ByteString
           | Bi BS.ByteString
           | Quad BS.ByteString

newtype Score a = Score a deriving (Num, Show, Eq)
newtype ErrorString a = ErrorString {runError :: a} deriving (Show, Eq)
newtype HexString a = HexString {runHex :: a} deriving (Show, Eq)
newtype Base64String a = Base64String {runBase64 :: a} deriving (Show, Eq)

-- Challenge 1

-- Convert hex to base64

-- The string:

-- 49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d

-- Should produce:

-- SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t

-- So go ahead and make that happen. You'll need to use this code for the rest of the exercises.
-- Cryptopals Rule

-- Always operate on raw bytes, never on encoded strings. Only use hex and base64 for pretty-printing.

hexToBase64 :: HexString BS.ByteString -> Either (ErrorString BS.ByteString) (Base64String BS.ByteString)
hexToBase64 a = Base64String . BS64.encode <$> decodeHex a

decodeHex :: HexString BS.ByteString -> Either (ErrorString BS.ByteString) BS.ByteString
decodeHex a | (decoded, "") <- BS16.decode . runHex $ a = Right decoded
            | otherwise = Left . ErrorString $ BS.concat ["invalid hex: ", runHex a]

-- Challenge 2
-- Fixed XOR

-- Write a function that takes two equal-length buffers and produces their XOR combination.

-- If your function works properly, then when you feed it the string:

-- 1c0111001f010100061a024b53535009181c

-- ... after hex decoding, and when XOR'd against:

-- 686974207468652062756c6c277320657965

-- ... should produce:

-- 746865206b696420646f6e277420706c6179

bytesToHex :: BS.ByteString -> HexString BS.ByteString
bytesToHex = HexString . BS16.encode

xorBytes :: BS.ByteString -> BS.ByteString -> Either (ErrorString BS.ByteString) BS.ByteString
xorBytes a b | BS.length a == BS.length b = Right . BS.pack $ BS.zipWith xor a b
             | otherwise = Left . ErrorString $ "Attempted to xor bytes of unequal length"

xorBytesByChar :: Word8 -> BS.ByteString ->  BS.ByteString
xorBytesByChar char str = BS.pack $ BS.zipWith xor (BS.pack charStr) str
  where
    charStr = replicate (fromIntegral . BS.length $ str) char

-- Single-byte XOR cipher

-- The hex encoded string:

-- 1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736

-- ... has been XOR'd against a single character. Find the key, decrypt the message.

-- You can do this by hand. But don't: write code to do it for you.

-- How? Devise some method for "scoring" a piece of English plaintext. Character frequency is a good metric. Evaluate each output and choose the one with the best score.
-- Achievement Unlocked

-- You now have our permission to make "ETAOIN SHRDLU" jokes on Twitter.

-- todo: replace list with sequence?

type IntermediateNGrams = ([BS.ByteString], (Int, [BS.ByteString]))

splitNGrams :: Int -> BS.ByteString -> [BS.ByteString]
splitNGrams n string = fst $ step emptyByte $ BS.foldr step initVal string

  where step :: Word8 -> IntermediateNGrams -> IntermediateNGrams
        step char (out, (0, parts)) = nextStep (last parts : out) 0 char (init parts)
        step char (out, (i, parts)) = nextStep out (i - 1) char parts

        nextStep :: [BS.ByteString] -> Int -> Word8 -> [BS.ByteString] -> IntermediateNGrams
        nextStep a b c d = (a, (b, BS.pack [c] : fmap (BS.cons c) d))

        initVal = ([], (n, []))

        emptyByte = BS.head " "

data ScoredPlaintext = ScoredPlaintext BS.ByteString (DecimalRaw Integer)
  deriving (Show, Eq)

-- TODO: change [MonoProb] to dict

scorePlaintext :: [MonoProb] -> BS.ByteString -> ScoredPlaintext
scorePlaintext probs str = ScoredPlaintext str productOfScores
  where
    productOfScores :: DecimalRaw Integer
    productOfScores = getProduct $ foldMap Product allProbabilityScores

    allProbabilityScores :: [DecimalRaw Integer]
    allProbabilityScores = getProbabilityScore <$> splitNGrams 1 str

    getProbabilityScore :: BS.ByteString -> DecimalRaw Integer
    getProbabilityScore query | Just (MonoProb _ prob) <- find (fetch query) probs = prob
                              | _                      <- find (fetch query) probs = Decimal 0 1

    fetch :: BS.ByteString -> MonoProb -> Bool
    fetch query (MonoProb ngram _) =  regexCaseIns (Ch8.singleton ngram) `Rx.matchTest` query


regexCaseIns :: BS.ByteString -> Rx.Regex
regexCaseIns = Rx.makeRegexOpts caseInsensitive Rx.defaultExecOpt
  where
    caseInsensitive = Rx.defaultCompOpt .|. Rx.compCaseless

monogramCountsRaw :: IO BSStrict.ByteString
monogramCountsRaw = BSStrict.readFile "english_monograms.txt"

data MonoCount = MonoCount Char Integer deriving (Show, Eq)
data MonoProb = MonoProb Char (DecimalRaw Integer) deriving (Show, Eq)

parseMonogramCount :: Tri.Parser MonoCount
parseMonogramCount = do
  mono <- letter
  space
  count <- natural
  pure $ MonoCount mono count

getMonoGramCounts :: BSStrict.ByteString -> Tri.Result [MonoCount]
getMonoGramCounts = parseByteString (some parseMonogramCount) mempty

calcProbabilityScores :: [MonoCount] -> [MonoProb]
calcProbabilityScores counts = fmap (countToProb totalCount) counts
  where
    totalCount :: Integer
    totalCount = foldr sumCounts 0 counts

    sumCounts :: MonoCount -> Integer -> Integer
    sumCounts (MonoCount _ a) b = a + b

countToProb :: Integer -> MonoCount -> MonoProb
countToProb total (MonoCount char count) = MonoProb char countOverTotal
  where
    countOverTotal = Decimal 0 count / Decimal 0 total
