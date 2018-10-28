{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module JsonConfigSpec where

import           Data.Aeson        (eitherDecode)
import           Data.Map          (empty, fromList)
import           Test.Hspec
import           Text.RawString.QQ
import           Types

spec :: IO ()
spec = hspec $
  describe "JsonConfig" $ do
    it "should decode ad prices" $ do
      let json = [r|{
          "prices": {
            "classic": 10000,
            "standout": 20000,
            "premium": 30000
          },
          "rules": {}
        }
        |]
      let result = eitherDecode json :: Either String Config
      result `shouldBe` Right (Config empty (fromList [(Classic, 100),(Standout, 200),(Premium, 300)]))
    it "should decode QuantityExtraAds rule" $ do
      let json = [r|{
          "prices": {},
          "rules": {
            "unilever": {
              "classic": {
                "type": "qtyExtraAds",
                "threshold": 2,
                "dealQty": 3
              }
            }
          }
        }
        |]
      let result = eitherDecode json :: Either String Config
      result `shouldBe` Right (Config (fromList [(Customer "unilever",fromList [(Classic, QuantityExtraAds (ThresholdQty 2) (DealQty 3))])]) empty)

    it "should decode QuantityPriceDrop rule" $ do
      let json = [r|{
          "prices": {},
          "rules": {
            "unilever": {
              "classic": {
                "type": "qtyPriceDrop",
                "threshold": 4,
                "price": 10000
              }
            }
          }
        }
        |]
      let result = eitherDecode json :: Either String Config
      result `shouldBe` Right (Config (fromList [(Customer "unilever",fromList [(Classic, QuantityPriceDrop (ThresholdQty 4) 100)])]) empty)

    it "should decode PriceDrop rule" $ do
      let json = [r|{
          "prices": {},
          "rules": {
            "unilever": {
              "classic": {
                "type": "priceDrop",
                "price": 10000
              }
            }
          }
        }
        |]
      let result = eitherDecode json :: Either String Config
      result `shouldBe` Right (Config (fromList [(Customer "unilever",fromList [(Classic, PriceDrop 100)])]) empty)
