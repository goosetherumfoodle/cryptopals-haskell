{-# LANGUAGE OverloadedStrings #-}

import qualified Test.Tasty
import Test.Tasty.Hspec
import Set1
import Data.ByteString
import Control.Monad (join)

main :: IO ()
main = do
    test <- testSpec "cryptopals" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  describe "Set1" $ do
    describe "Challenge 1" $ do
      it "translates hex string to correct base64 string" $ do
        let expected = Right . Base64String $ "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
            result = hexToBase64 $ HexString "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        result `shouldBe` expected
    describe "Challenge 2" $ do
      it "correctly XORs hex strings" $ do
        let firstBytes = decodeHex . HexString $ "1c0111001f010100061a024b53535009181c"
            secondBytes = decodeHex . HexString $ "686974207468652062756c6c277320657965"
        (bytesToHex <$> join (xor <$> firstBytes <*> secondBytes)) `shouldBe` (Right . HexString $ ("746865206b696420646f6e277420706c6179" :: ByteString))
