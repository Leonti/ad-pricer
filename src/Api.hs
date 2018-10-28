{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module Api where

import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics
import           Types

data CardRequest = CardRequest
  { customer :: Customer
  , skus     :: [Ad]
  } deriving (Show, Generic, FromJSON)

newtype CheckoutResponse = CheckoutResponse
  { total :: Integer } deriving (Show, Generic, ToJSON)
