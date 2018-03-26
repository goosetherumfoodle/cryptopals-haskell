{-# LANGUAGE OverloadedStrings #-}

import qualified Test.Tasty
import Test.Tasty.Hspec
import Set1
import Data.ByteString.Lazy
import Control.Monad (join)
import qualified Text.Trifecta as Tri
import           Data.Decimal              (DecimalRaw (Decimal))

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

      describe "scoring plaintext" $ do
        it "finds product of probability scores of each monogram" $ do
          let probabilities = [[MonoProb 'H' (Decimal 1 4), MonoProb 'O' (Decimal 1 2)]]
              plain = "horse"
          scorePlaintext probabilities plain `shouldBe` ScoredPlaintext plain (Decimal 2 8)

        it "finds product of probability scores of each ngram" $ do
          let probabilities = [[MonoProb 'H' (Decimal 1 2)]
                             , [BiProb "HO" (Decimal 1 2) , BiProb "XX" (Decimal 0 5)]
                             , [TriProb "RSE" (Decimal 1 2)]
                             , [QuadProb "HORS" (Decimal 1 2)]]
              plain = "horse"
          scorePlaintext probabilities plain `shouldBe` ScoredPlaintext plain (Decimal 4 16)

      describe "parsing ngram counts" $ do
        context "with a valid mongrams + counts  string" $ do
          it "successfully parses MonoRaw" $ do
            let input = "E 529117365\nT 390965105"
                expected = Tri.Success [MonoRaw 'E' 529117365, MonoRaw 'T' 390965105]
            getMonoGramCounts input `shouldBe` expected

        context "with a valid bigrams + counts  string" $ do
          it "successfully parses BiCounts" $ do
            let input = "AB 529117365\nCD 390965105"
                expected = Tri.Success [BiRaw ('A', 'B') 529117365, BiRaw ('C', 'D') 390965105]
            getBiGramCounts input `shouldBe` expected

        context "with a valid trigrams + counts  string" $ do
          it "successfully parses TriCounts" $ do
            let input = "ABC 529117365\nDEF 390965105"
                expected = Tri.Success [TriRaw ('A', 'B', 'C') 529117365, TriRaw ('D', 'E', 'F') 390965105]
            getTriGramCounts input `shouldBe` expected

        context "with a valid quadgrams + counts  string" $ do
          it "successfully parses QuadCounts" $ do
            let input = "ABCD 529117365\nEFGH 390965105"
                expected = Tri.Success [QuadRaw ('A', 'B', 'C', 'D') 529117365, QuadRaw ('E', 'F', 'G', 'H') 390965105]
            getQuadGramCounts input `shouldBe` expected

        context "with a invalid mongrams + counts string" $ do
          it "fails to parse Monocounts" $ do
            let input = "E 529117365\nTR 390965105"
            shouldBeTriFailure $ getMonoGramCounts input

      describe "deriving ngram probability scores" $ do
        context "with a monogram that occurs 2 times, out of 5 total monograms" $ do
          it "should have a probability score of 0.4" $ do
            let input = [MonoCount 'A' 2, MonoCount 'T' 3]
                expected = [MonoProb 'A' (Decimal 1 4), MonoProb 'T' (Decimal 1 6)]
            expected `shouldBe` calcProbabilityScores input

        context "with a bigram that occurs 2 times, out of 5 total bigrams" $ do
          it "should have a probability score of 0.4" $ do
            let input = [BiCount "AA" 2, BiCount "TT" 3]
                expected = [BiProb "AA" (Decimal 1 4), BiProb "TT" (Decimal 1 6)]
            expected `shouldBe` calcProbabilityScores input


-- helpers for tests

shouldBeTriFailure :: Tri.Result a -> Bool
shouldBeTriFailure (Tri.Failure _) = True
shouldBeTriFailure (Tri.Success _) = False

instance Eq a => Eq (Tri.Result a) where
  (Tri.Success a) == (Tri.Success b) = a == b
  (Tri.Failure _) == (Tri.Failure _) = False
  _ == _ = False
