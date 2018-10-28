{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Types where

import           Data.Aeson       (FromJSON (..), FromJSONKey,
                                   FromJSONKeyFunction (..), withObject,
                                   withText, (.:))
import           Data.Aeson.Types (FromJSONKey (..), Parser)
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Ratio       ((%))
import           Data.Text        (Text, unpack)

data Ad =
    Classic
  | Standout
  | Premium deriving (Eq, Ord, Show)

newtype ThresholdQty = ThresholdQty Int deriving (Eq, Show, FromJSON)
newtype DealQty = DealQty Int deriving (Eq, Show, FromJSON)

data Deal =
    QuantityExtraAds ThresholdQty DealQty
  | QuantityPriceDrop ThresholdQty Rational
  | PriceDrop Rational
  deriving (Eq, Show)

data PriceQty = PriceQty Rational Int deriving (Eq, Show)
type PricingRules = Map Ad Deal
type CheckoutItems = Map Ad PriceQty
data Checkout = Checkout PricingRules CheckoutItems deriving (Eq, Show)

newtype Customer = Customer String deriving (Eq, Ord, Show, FromJSONKey, FromJSON)

data Config = Config (Map Customer PricingRules) (Map Ad Rational) deriving (Eq, Show)

parseAd :: Text -> Parser Ad
parseAd x = case x of
  "classic"  -> return Classic
  "standout" -> return Standout
  "premium"  -> return Premium
  _          -> fail $ "Unknown ad type: '" <> unpack x <> "'"

instance FromJSON Ad where
  parseJSON = withText "ad" parseAd

instance FromJSONKey Ad where
  fromJSONKey = FromJSONKeyTextParser parseAd

instance FromJSON Deal where
  parseJSON = withObject "Deal" $ \o -> do
    dealType <- o .: "type" :: Parser String
    case dealType of
      "qtyExtraAds"  -> QuantityExtraAds <$> o .: "threshold" <*> o .: "dealQty"
      "qtyPriceDrop" -> QuantityPriceDrop <$> o .: "threshold" <*> ((% 100) <$> (o .: "price":: Parser Integer))
      "priceDrop"    -> PriceDrop <$> ((% 100) <$> (o .: "price":: Parser Integer))
      _              -> fail $ "Unknown deal type: '" <> dealType <> "'"

instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    prices <- o .: "prices" :: Parser (Map Ad Integer)
    rules <- o .: "rules" :: Parser (Map Customer PricingRules)

    return $ Config rules (Map.map (% 100) prices)
