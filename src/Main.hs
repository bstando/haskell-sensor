{-# LANGUAGE OverloadedStrings #-}
module Main where


import Control.Applicative
import Sensor
import EspHandler
import ChartGenerator


main :: IO()
main = makeChart
