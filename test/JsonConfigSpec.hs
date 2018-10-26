{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module JsonConfigSpec where

import           Data.Aeson        (decode)
import           Data.Map          (empty, fromList)
import           Test.Hspec
import           Text.RawString.QQ
import           Types

spec :: IO ()
spec = hspec $ do
  describe "JsonConfig" $ do
    it "should decode ad prices" $ do
      let json = [r|{
        "prices": {
          "classic": 26999,
          "standout": 32299,
          "premium": 39499
        },
        "rules": {}
      }
      |]
      let result = decode json :: Maybe Config
      result `shouldBe` Just (Config empty empty)
