module ChartGenerator where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Control.Monad.IO.Class
import Control.Monad.Trans (liftIO)
import Data.Time.LocalTime
import Data.Time.Calendar
import Data.Time.LocalTime
import Sensor

mkDate yyyy mm dd hh mn ss = LocalTime (fromGregorian yyyy mm dd)
                         (dayFractionToTimeOfDay (((hh*60+mn)*60+ss)/(1440*60)))


readed =  do
          sensor <- getSensorData
          let cnv = map (\x -> toArray x) sensor
          let read = [ (mkDate (fromIntegral yyyy) mm dd (toRational hh) (toRational mn) (toRational ss), temp, hum) | (_,yyyy,mm,dd,hh,mn,ss,temp,hum) <- cnv ]
          return read

makeChart = do
            readings <- readed
            toFile def "data.png" $ do
            layoutlr_title .= "Odczyty z sensora"
            layoutlr_left_axis . laxis_override .= axisGridHide
            layoutlr_right_axis . laxis_override .= axisGridHide
            plotLeft (line "Temperatura" [ [ (d,v) | (d,v,_) <- readings] ])
            plotRight (line "Wilgotność" [ [ (d,v) | (d,_,v) <- readings] ])
