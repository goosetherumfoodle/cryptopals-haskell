{-# LANGUAGE OverloadedStrings #-}

import qualified Test.Tasty
import Test.Tasty.Hspec
import Set1
import Data.ByteString.Lazy
import Control.Monad (join)
import qualified Text.Trifecta as Tri

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
        (bytesToHex <$> join (xorBytes <$> firstBytes <*> secondBytes)) `shouldBe` (Right . HexString $ ("746865206b696420646f6e277420706c6179" :: ByteString))
    describe "Challenge 3" $ do
      describe "splitNGrams" $ do
        describe "collects all N groups in list" $ do
          it "when N is 2, finds all bigrams" $ do
            (splitNGrams 2 "12345") `shouldBe` ["12", "23", "34", "45"]

          it "when N is 3, finds all trigrams" $ do
            (splitNGrams 3 "12345") `shouldBe` ["123", "234", "345"]

      describe "occuranceCount" $ do
        context "with two occurances of the ngram (mixed case)" $ do
          it "finds 2 occurances" $ do
            let string = "the boy GROWETH"
                ngram = "th"
            occuranceCount ngram string `shouldBe` 2

        context "with no occurances in string" $ do
          it "finds 0 occurances" $ do
            let string = "hte boy GROWET How"
                ngram = "th"
            occuranceCount ngram string `shouldBe` 0

      describe "scoring plaintext" $ do
        it "adds 1 to score for each present monogram" $
          scorePlaintext [((fmap Mono) ["a", "o", "r", "x"])] "horse" `shouldBe` Score 2

        it "adds 1 for monograms, 2 for bigrams and 3 for quadgrams" $
          scorePlaintext [(Mono <$> ["a", "o", "r", "x"])
                         , (Bi <$> ["rs", "th", "oh"])
                         , (Quad <$> ["orse", "xrse"])] "horse" `shouldBe` Score 7

      describe "parsing monogram counts" $ do
        context "with a valid mongrams + counts  string" $ do
          it "successfully parses MonoCounts" $ do
            let input = "E 529117365\nT 390965105"
                expected = Tri.Success [MonoCount 'E' 529117365, MonoCount 'T' 390965105]
            getMonoGramCounts input `shouldBe` expected

        context "with a invalid mongrams + counts string" $ do
          it "fails to parse Monocounts" $ do
            let input = "E 529117365\nTR 390965105"
            shouldBeTriFailure $ getMonoGramCounts input

-- helpers for tests

shouldBeTriFailure :: Tri.Result a -> Bool
shouldBeTriFailure (Tri.Failure _) = True
shouldBeTriFailure (Tri.Success _) = False

instance Eq a => Eq (Tri.Result a) where
  (Tri.Success a) == (Tri.Success b) = a == b
  (Tri.Failure _) == (Tri.Failure _) = False
  _ == _ = False
