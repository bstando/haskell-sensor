{-# LANGUAGE OverloadedStrings #-}
module Sensor where

import Data.Monoid
{- import Control.Monad.IO.Class -}
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Applicative
import Data.Maybe 
import EspHandler
import System.Time
import Data.Text.Lazy as T (pack,concat,Text)

data SensorData = SensorData { id :: Int, year :: Int, month :: Int, day :: Int, hours :: Int, minutes :: Int, seconds :: Int, temperature :: Float, humidity :: Float } deriving (Show)

toArray :: SensorData -> (Int, Int, Int, Int, Int, Int, Int, Float, Float)
toArray (SensorData id year month day hours minutes seconds temperature humidity) = (id, year, month, day, hours, minutes, seconds, temperature, humidity)

monthToInt :: Month -> Int
monthToInt month | month == January = 01
                 | month == February = 02
                 | month == March = 03
                 | month == April = 04
                 | month == May = 05
                 | month == June = 06
                 | month == July = 07
                 | month == August = 08
                 | month == September = 09
                 | month == October = 10
                 | month == November = 11
                 | month == December = 12


instance FromRow SensorData where 
  fromRow = SensorData <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field
instance ToRow SensorData where 
  toRow (SensorData _ year month day hours minutes seconds temperature humidity) = toRow (year, month, day, hours, minutes, seconds, temperature, humidity);

getSensorData :: IO [SensorData]
getSensorData = do
                conn <- open "sensor.db"
                r <- query_ conn "SELECT * from sensor" :: IO [SensorData]
                close conn
                return r

saveSensorData :: String -> IO ()
saveSensorData addr = do
		 conn <- open "sensor.db"
                 fromEsp <- getEspData addr
                 let r = fromJust fromEsp
		 now <- getClockTime
		 nowCal <- toCalendarTime now
                 execute conn "INSERT INTO sensor (year, month, day, hours, minutes, seconds, temperature, humidity) values (?, ?, ?, ?, ?, ?, ?, ?)" (SensorData 1 (ctYear nowCal) (monthToInt (ctMonth nowCal)) (ctDay nowCal) (ctHour nowCal) (ctMin nowCal) (ctSec nowCal) (temperatureEspData r) (humidityEspData r))
                 close conn
                 
