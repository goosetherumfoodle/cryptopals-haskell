{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Set1 where

import Data.Word (Word8)
import qualified Data.ByteString        as BSStrict
import qualified Data.ByteString.Lazy        as BS
import qualified Data.ByteString.Base16.Lazy as BS16
import qualified Data.ByteString.Base64.Lazy as BS64
import Data.Bits ((.|.), xor)
import Text.Regex.PCRE  (defaultExecOpt, compCaseless, defaultCompOpt, makeRegexOpts, matchCount)
import qualified Text.Trifecta as Tri
import Text.Trifecta (letter, space, natural, some, parseByteString)

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

scorePlaintext :: [[NGram]] -> BS.ByteString -> Score Int
scorePlaintext ngrams s = foldr (nGramsScore s) 0 ngrams

nGramsScore :: BS.ByteString -> [NGram] -> Score Int -> Score Int
nGramsScore str ngrams initScore = foldr step initScore ngrams
  where
    step :: NGram -> Score Int -> Score Int
    step (Mono gram) score = score + Score (occuranceCount gram str) * 1
    step (Bi gram) score   = score + Score (occuranceCount gram str) * 2
    step (Quad gram) score = score + Score (occuranceCount gram str) * 3

occuranceCount :: BS.ByteString -> BS.ByteString -> Int
occuranceCount = matchCount . regex
  where
    regex = makeRegexOpts caseInsensitive defaultExecOpt
    caseInsensitive = defaultCompOpt .|. compCaseless

monogramCountsRaw :: IO BSStrict.ByteString
monogramCountsRaw = BSStrict.readFile "english_monograms.txt"

newtype Count a = Count a deriving (Show, Eq, Num)
newtype Prob a = Prob a deriving Show

data MonoCount = MonoCount Char (Count Integer) deriving (Show, Eq)
data MonoProb = MonoProp Char (Prob Integer)

parseMonogramCount :: Tri.Parser MonoCount
parseMonogramCount = letter >>= \mono -> space >> natural >>= \c -> pure . MonoCount mono $ Count c

getMonoGramCounts :: BSStrict.ByteString -> Tri.Result [MonoCount]
getMonoGramCounts = parseByteString (some parseMonogramCount) mempty
