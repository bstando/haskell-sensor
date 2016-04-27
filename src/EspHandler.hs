{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module EspHandler where

import Data.Aeson
import GHC.Generics
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)

data EspData = 
     EspData { temperature   :: Float
               , humidity    :: Float
             } deriving (Show, Generic)  

temperatureEspData :: EspData -> Float
temperatureEspData (EspData temperature _ ) = temperature

humidityEspData :: EspData -> Float
humidityEspData (EspData _ humidity ) = humidity

instance FromJSON EspData
instance ToJSON EspData

getJSON :: String -> IO B.ByteString
getJSON jsonURL = simpleHttp jsonURL

getEspData :: [Char] -> IO (Maybe EspData)
getEspData jsonURL = fmap decode $ getJSON $ jsonURL
