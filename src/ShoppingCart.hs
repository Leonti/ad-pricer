module ShoppingCart(
  module Types,
  checkout
  ) where

import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)
import           Types

checkout :: Config -> Customer -> [Ad] -> Either String Checkout
checkout (Config ruleMap adMap) customer ads = Checkout customerRules <$> items
  where
    customerRules = fromMaybe Map.empty $ Map.lookup customer ruleMap
    qtyMap = foldr (\ad acc -> Map.insertWith (+) ad 1 acc) Map.empty ads
    items = Map.traverseWithKey (\ad qty -> (`PriceQty` qty) <$> findPrice ad) qtyMap
    findPrice ad = case Map.lookup ad adMap of
      Just price -> Right price
      Nothing    -> Left $ "Config for " <> show ad <> " is missing"
