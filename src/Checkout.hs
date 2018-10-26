module Checkout(
  module Types,
  calculateTotal
  ) where

import           Data.Map (foldrWithKey)
import qualified Data.Map as Map
import           Types

calculateTotal :: Checkout -> Rational
calculateTotal (Checkout rules items) = foldrWithKey (priceWithRules rules) 0 items

priceWithRules :: PricingRules -> Ad -> PriceQty -> Rational -> Rational
priceWithRules rules ad priceQty acc =
  acc + maybe (itemTotal priceQty) (applyDeal priceQty) (Map.lookup ad rules)

applyDeal :: PriceQty -> Deal -> Rational
applyDeal p@(PriceQty price qty) (QuantityExtraAds (ThresholdQty tQty) (DealQty dQty))
  | qty > tQty = price * fromIntegral ((qty `div` dQty) * tQty + (qty `mod` dQty))
  | otherwise = itemTotal p
applyDeal p@(PriceQty _ qty) (QuantityPriceDrop (ThresholdQty tQty) newPrice)
  | qty >= tQty = newPrice * fromIntegral qty
  | otherwise = itemTotal p
applyDeal (PriceQty _ qty) (PriceDrop newPrice) = newPrice * fromIntegral qty

itemTotal :: PriceQty -> Rational
itemTotal (PriceQty price qty) = price * fromIntegral qty
