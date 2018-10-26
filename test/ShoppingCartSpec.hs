module ShoppingCartSpec where

import           Data.Map     (empty, fromList)
import           ShoppingCart
import           Test.Hspec

spec :: IO ()
spec = hspec $ do
  describe "Shopping Cart items" $ do
    it "returns empty checkout if no ads are in config" $
      checkout (Config empty empty) (Customer "default") [Classic]
      `shouldBe`
      (Left "Config for Classic is missing")
    it "returns empty checkout if no ads are in the shopping cart" $
      checkout (Config empty (fromList [(Classic, 100)])) (Customer "default") []
      `shouldBe`
      (Right $ Checkout empty empty)
    it "returns single item with a correct price" $
      checkout (Config empty (fromList [(Classic, 100)])) (Customer "default") [Classic]
      `shouldBe`
      (Right $ Checkout empty (fromList [(Classic, PriceQty 100 1)]))
    it "increases count for duplicate items" $
      checkout (Config empty (fromList [(Classic, 100)])) (Customer "default") [Classic, Classic]
      `shouldBe`
      (Right $ Checkout empty (fromList [(Classic, PriceQty 100 2)]))
  describe "Shopping Cart pricing rules" $ do
    it "finds rules for a customer" $
      checkout (Config
        (fromList [(Customer "nike", fromList [(Classic, PriceDrop 70)])])
        (fromList [(Classic, 100)])) (Customer "nike") [Classic]
      `shouldBe`
      (Right $ Checkout (fromList [(Classic, PriceDrop 70)]) (fromList [(Classic, PriceQty 100 1)]))
    it "finds no rules if customer name doesn't match" $
      checkout (Config
        (fromList [(Customer "nike", fromList [(Classic, PriceDrop 70)])])
        (fromList [(Classic, 100)])) (Customer "default") [Classic]
      `shouldBe`
      (Right $ Checkout empty (fromList [(Classic, PriceQty 100 1)]))
