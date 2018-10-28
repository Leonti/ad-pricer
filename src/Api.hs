{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api(runServer) where

import           Data.Aeson               (FromJSON, ToJSON, eitherDecode)
import           Data.Proxy
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Types

import           Checkout                 (calculateTotal)
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy     as LB
import           Data.Text.Lazy           as TL
import           Data.Text.Lazy.Encoding  as TLE
import           Servant
import           ShoppingCart             (checkout)

data ShoppingCartRequest = ShoppingCartRequest
  { customer :: Customer
  , skus     :: [Ad]
  } deriving (Show, Generic, FromJSON)

newtype CheckoutResponse = CheckoutResponse
  { total :: Rational } deriving (Show, Generic, ToJSON)

type Api = "checkout" :> ReqBody '[JSON] ShoppingCartRequest :> Post '[JSON] CheckoutResponse

checkoutHandler :: ShoppingCartRequest -> Handler CheckoutResponse
checkoutHandler (ShoppingCartRequest c ads) = do
  jsonConfig <- liftIO $ LB.readFile "config.json"
  let resp = do
              config <- eitherDecode jsonConfig :: Either String Config
              calculateTotal <$> checkout config c ads
  case resp of
    Right t -> return $ CheckoutResponse t
    Left e      -> Handler (throwError $ err400 { errBody = (TLE.encodeUtf8 . TL.pack) e })


api :: Proxy Api
api = Proxy

server :: Server Api
server = checkoutHandler

apiApp :: Application
apiApp = serve api server

runServer :: Port -> IO ()
runServer port = run port apiApp
