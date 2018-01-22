{-# LANGUAGE OverloadedStrings #-}

import qualified Test.Tasty
import Test.Tasty.Hspec
import Set1


main :: IO ()
main = do
    test <- testSpec "cryptopals" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  describe "Set1" $ do
    describe "hexToBase64" $ do
      it "translates hex string to correct base64 string" $ do
        let expected = Right . Base64String $ "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
            result = hexToBase64 $ HexString "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
        result `shouldBe` expected
