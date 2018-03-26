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
import Control.Monad (join)
import Debug.Trace (trace)
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
        step char (out, (0, parts)) = nextStep (last parts : out) 0 char $ init parts
        step char (out, (i, parts)) = nextStep out (i - 1) char parts

        nextStep :: [BS.ByteString] -> Int -> Word8 -> [BS.ByteString] -> IntermediateNGrams
        nextStep a b c d = (a, (b, BS.pack [c] : fmap (BS.cons c) d))

        initVal :: IntermediateNGrams
        initVal = ([], (n, []))

        emptyByte = BS.head " "

data ScoredPlaintext = ScoredPlaintext BS.ByteString (DecimalRaw Integer)
  deriving (Show, Eq)

-- TODO: change [MonoProb] to dict for O(1) lookup

scorePlaintext :: [[NGramProb]] -> BS.ByteString -> ScoredPlaintext
scorePlaintext probs str = ScoredPlaintext str productOfScores
  where
    productOfScores :: DecimalRaw Integer
    productOfScores = getProduct . foldMap Product . join $ allProbabilityScores <$> probs

    allProbabilityScores :: [NGramProb] -> [DecimalRaw Integer]
    allProbabilityScores probs'@((MonoProb _ _):_) = (getProbabilityScore probs') <$> splitNGrams 1 str
    allProbabilityScores probs'@((BiProb _ _):_) = (getProbabilityScore probs') <$> splitNGrams 2 str
    allProbabilityScores probs'@((TriProb _ _):_) = (getProbabilityScore probs') <$> splitNGrams 3 str
    allProbabilityScores probs'@((QuadProb _ _):_) = (getProbabilityScore probs') <$> splitNGrams 4 str
    allProbabilityScores _ = [emptyDecimal]

    getProbabilityScore :: [NGramProb] -> BS.ByteString -> DecimalRaw Integer
    getProbabilityScore probs'' query | Just prob <- find (fetch query) probs'' = probScore prob
                                      | _         <- find (fetch query) probs'' = emptyDecimal

    fetch :: BS.ByteString -> NGramProb -> Bool
    fetch query prob = regexCaseIns (probNGram prob) `Rx.matchTest` query

    emptyDecimal = Decimal 0 1

probScore :: NGramProb -> DecimalRaw Integer
probScore (MonoProb _ score) = score
probScore (BiProb _ score)   = score
probScore (TriProb _ score)  = score
probScore (QuadProb _ score) = score

probNGram :: NGramProb -> BS.ByteString
probNGram (MonoProb ngram _) = Ch8.singleton ngram
probNGram (BiProb ngram _)   = ngram
probNGram (TriProb ngram _)  = ngram
probNGram (QuadProb ngram _) = ngram

regexCaseIns :: BS.ByteString -> Rx.Regex
regexCaseIns = Rx.makeRegexOpts caseInsensitive Rx.defaultExecOpt
  where
    caseInsensitive = Rx.defaultCompOpt .|. Rx.compCaseless

monogramCountsRaw :: IO BSStrict.ByteString
monogramCountsRaw = BSStrict.readFile "english_monograms.txt"

-- for length-checked storage
data RawMonoGramCount = MonoRaw Char Integer deriving (Show, Eq)

data RawBiGramCount = BiRaw (Char, Char) Integer deriving (Show, Eq)

data RawTriGramCount = TriRaw (Char, Char, Char) Integer deriving (Show, Eq)

data RawQuadGramCount = QuadRaw (Char, Char, Char, Char) Integer deriving (Show, Eq)

data NGramCount = MonoCount Char Integer
                | BiCount BS.ByteString Integer
                | TriCount BS.ByteString Integer
                | QuadCount BS.ByteString Integer deriving (Show, Eq)

data NGramProb = MonoProb Char (DecimalRaw Integer)
                | BiProb BS.ByteString (DecimalRaw Integer)
                | TriProb BS.ByteString (DecimalRaw Integer)
                | QuadProb BS.ByteString (DecimalRaw Integer) deriving (Show, Eq)

parseRawMonogramCount :: Tri.Parser RawMonoGramCount
parseRawMonogramCount = do
  mono <- letter
  parseCount $ MonoRaw mono

parseRawBigramCount :: Tri.Parser RawBiGramCount
parseRawBigramCount = do
  one <- letter
  two <- letter
  parseCount $ BiRaw (one, two)

parseRawTrigramCount :: Tri.Parser RawTriGramCount
parseRawTrigramCount = do
  one <- letter
  two <- letter
  three <- letter
  parseCount $ TriRaw (one, two, three)

parseRawQuadgramCount :: Tri.Parser RawQuadGramCount
parseRawQuadgramCount = do
  one <- letter
  two <- letter
  three <- letter
  four <- letter
  parseCount $ QuadRaw (one, two, three, four)

parseCount :: (Integer -> a) -> Tri.Parser a
parseCount cstr = space >> natural >>= \count -> return $ cstr count

getMonoGramCounts :: BSStrict.ByteString -> Tri.Result [RawMonoGramCount]
getMonoGramCounts = parseSome parseRawMonogramCount

getBiGramCounts :: BSStrict.ByteString -> Tri.Result [RawBiGramCount]
getBiGramCounts = parseSome parseRawBigramCount

getTriGramCounts :: BSStrict.ByteString -> Tri.Result [RawTriGramCount]
getTriGramCounts = parseSome parseRawTrigramCount

getQuadGramCounts :: BSStrict.ByteString -> Tri.Result [RawQuadGramCount]
getQuadGramCounts = parseSome parseRawQuadgramCount

parseSome :: Tri.Parser a -> BSStrict.ByteString -> Tri.Result [a]
parseSome = flip parseByteString mempty . some

calcProbabilityScores :: [NGramCount] -> [NGramProb]
calcProbabilityScores counts = fmap (countToProb totalCount) counts
  where
    totalCount :: Integer
    totalCount = foldr sumCounts 0 counts

    sumCounts :: NGramCount -> Integer -> Integer
    sumCounts (MonoCount _ a) b = a + b
    sumCounts (BiCount _ a) b = a + b
    sumCounts (TriCount _ a) b = a + b
    sumCounts (QuadCount _ a) b = a + b

countToProb :: Integer -> NGramCount -> NGramProb
countToProb total (MonoCount char count) = mkProb MonoProb char total count
countToProb total (BiCount bigram count) = mkProb BiProb bigram total count
countToProb total (TriCount trigram count) = mkProb TriProb trigram total count
countToProb total (QuadCount quadgram count) = mkProb QuadProb quadgram total count

type Total = Integer
type Count = Integer

mkProb :: (a -> DecimalRaw Integer -> NGramProb) -> a -> Total -> Count -> NGramProb
mkProb constructor gram total count = constructor gram countOverTotal
  where
    countOverTotal = Decimal 0 count / Decimal 0 total
