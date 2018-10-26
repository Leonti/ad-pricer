module CheckoutSpec where

import           Checkout
import           Data.Map              (empty, fromList)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Checkout" $ do
    it "0 dollars on empty data" $
      calculateTotal (Checkout empty empty) `shouldBe` 0
    it "sum of ads without rules" $
      calculateTotal (Checkout empty $ fromList [(Classic, PriceQty 100 2)]) `shouldBe` 200
    it "applies a rule when matched" $
      calculateTotal (Checkout
         (fromList [ (Classic, PriceDrop 70) ])
        $ fromList [ (Classic, PriceQty 100 2) ]) `shouldBe` 140
    it "does not apply an unmatched rule" $
      calculateTotal (Checkout
         (fromList [ (Standout, PriceDrop 70) ])
        $ fromList [ (Classic, PriceQty 100 2) ]) `shouldBe` 200
  describe "Extra ads deal" $ do
    it "keeps the same price if the purchased number is the same as threshold quantity" $
      calculateTotal (Checkout
         (fromList [ (Premium, QuantityExtraAds (ThresholdQty 2) (DealQty 3)) ])
        $ fromList [ (Premium, PriceQty 100 2) ]) `shouldBe` 200
    it "adjusts price if the purchased number is the same as deal quantity" $
      calculateTotal (Checkout
        (fromList [ (Premium, QuantityExtraAds (ThresholdQty 1) (DealQty 3)) ])
        $ fromList [ (Premium, PriceQty 100 3) ]) `shouldBe` 100
    it "applies deal multiple times" $
      calculateTotal (Checkout
        (fromList [ (Premium, QuantityExtraAds (ThresholdQty 1) (DealQty 3)) ])
        $ fromList [ (Premium, PriceQty 100 10) ]) `shouldBe` 400
    it "deal is not applied for the remaining items if threshold is not reached" $
      calculateTotal (Checkout
        (fromList [ (Premium, QuantityExtraAds (ThresholdQty 1) (DealQty 3)) ])
        $ fromList [ (Premium, PriceQty 100 8) ]) `shouldBe` 400
  describe "Quantity price drop" $ do
    it "keeps the same price if the purchased number is less than threshold quantity" $
      calculateTotal (Checkout
        (fromList [ (Premium, QuantityPriceDrop (ThresholdQty 2) 70) ])
        $ fromList [ (Premium, PriceQty 100 1) ]) `shouldBe` 100
    it "applies deal if purchased quantity is the same as threshold quantity" $
      calculateTotal (Checkout
        (fromList [ (Premium, QuantityPriceDrop (ThresholdQty 2) 70) ])
        $ fromList [ (Premium, PriceQty 100 2) ]) `shouldBe` 140
    it "applies deal if purchased quantity is bigger than threshold quantity" $
      calculateTotal (Checkout
        (fromList [ (Premium, QuantityPriceDrop (ThresholdQty 2) 70) ])
        $ fromList [ (Premium, PriceQty 100 3) ]) `shouldBe` 210
