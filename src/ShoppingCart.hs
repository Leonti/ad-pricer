module ShoppingCart(
  module Types,
  checkout
  ) where

import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)
import           Types

checkout :: Config -> Customer -> [Ad] -> Checkout
checkout (Config ruleMap adMap) customer ads = Checkout customerRules items
  where
    customerRules = fromMaybe Map.empty $ Map.lookup customer ruleMap
    qtyMap = foldr (\ad acc -> Map.insertWith (+) ad 1 acc) Map.empty ads
    items = Map.mapMaybeWithKey (\ad qty -> fmap (`PriceQty` qty) (Map.lookup ad adMap)) qtyMap
