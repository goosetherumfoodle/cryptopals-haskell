{-# LANGUAGE OverloadedStrings #-}

module Set1 where

import Data.Word (Word8)
import           Data.ByteString.Lazy        (ByteString)
import qualified Data.ByteString.Lazy        as BS
import qualified Data.ByteString.Base16.Lazy as BS16
import qualified Data.ByteString.Base64.Lazy as BS64
import Data.Bits (xor)
import Debug.Trace

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

hexToBase64 :: HexString ByteString -> Either (ErrorString ByteString) (Base64String ByteString)
hexToBase64 a = Base64String . BS64.encode <$> (decodeHex a)

decodeHex :: HexString ByteString -> Either (ErrorString ByteString) ByteString
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

bytesToHex :: ByteString -> HexString ByteString
bytesToHex = HexString . BS16.encode

xorBytes :: ByteString -> ByteString -> Either (ErrorString ByteString) ByteString
xorBytes a b | (BS.length a) == (BS.length b) = Right . BS.pack $ BS.zipWith xor a b
             | otherwise = Left . ErrorString $ "Attempted to xor bytes of unequal length"

-- Single-byte XOR cipher

-- The hex encoded string:

-- 1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736

-- ... has been XOR'd against a single character. Find the key, decrypt the message.

-- You can do this by hand. But don't: write code to do it for you.

-- How? Devise some method for "scoring" a piece of English plaintext. Character frequency is a good metric. Evaluate each output and choose the one with the best score.
-- Achievement Unlocked

-- You now have our permission to make "ETAOIN SHRDLU" jokes on Twitter.
-- foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
findNgrams :: Int -> ByteString -> [ByteString]
findNgrams n string = outString $ BS.foldr (go $ n - 1) (n, mempty, []) string

go :: Int -> Word8 -> (Int, [Word8], [ByteString]) -> (Int, [Word8], [ByteString])
go x char tup@(0, string, out) = trace ("first " ++ show tup ++ " -> " ++ show horse ) horse where horse = (x, [char], BS.pack string : out)
go _ char tup@(y, string, out) = trace ("second" ++ show tup ++ " -> " ++ show horse) horse where horse = (y - 1, char : string, out)

outString :: (a, b, c) -> c
outString (_, _, s) = s
