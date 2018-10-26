module Types where

import           Data.Aeson (FromJSON (..), withObject)
import           Data.Map   (Map)
import qualified Data.Map   as Map

data Ad =
    Classic
  | Standout
  | Premium deriving (Eq, Ord, Show)

newtype ThresholdQty = ThresholdQty Int deriving (Eq, Show)
newtype DealQty = DealQty Int deriving (Eq, Show)

data Deal =
    QuantityExtraAds ThresholdQty DealQty
  | QuantityPriceDrop ThresholdQty Rational
  | PriceDrop Rational
  deriving (Eq, Show)

data PriceQty = PriceQty Rational Int deriving (Eq, Show)
type PricingRules = Map Ad Deal
type CheckoutItems = Map Ad PriceQty
data Checkout = Checkout PricingRules CheckoutItems deriving (Eq, Show)

newtype Customer = Customer String deriving (Eq, Ord, Show)

data Config = Config (Map Customer PricingRules) (Map Ad Rational) deriving (Eq, Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v ->
    return $ Config Map.empty Map.empty
