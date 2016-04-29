{-# LANGUAGE TemplateHaskell #-}
module GUI where

import Graphics.UI.Gtk as G
import Graphics.UI.Gtk.Builder
import ChartGenerator
import Control.Concurrent
import Data.Maybe
import EspHandler
import Sensor
import Control.Monad
import System.IO
import Control.Lens as L
import Control.Concurrent
import Control.Concurrent.STM

data Fork = Fork { _threadId :: ThreadId }
makeLenses ''Fork

idFork :: Fork -> ThreadId
idFork (Fork _threadId) = _threadId

setThread :: Fork -> ThreadId -> Fork
setThread fork thread = fork { _threadId = thread } 

data State = State {working :: TVar Int}

workingState (State working) = working



showMainWindow gladepath = do
                           value <-  newTVarIO 0 :: IO (TVar Int)
			   let thread = State value
                           initGUI
                           builder <- builderNew
                           builderAddFromFile builder gladepath
                           mainWindow <- builderGetObject builder castToWindow "mainWindow"
                           relayOnButton <- builderGetObject builder castToButton "relayOn"
                           relayOffButton <- builderGetObject builder castToButton "relayOff"
                           sensorDialog  <-  builderGetObject builder castToDialog "sensorDialog"
                           sensorOKButton  <-  builderGetObject builder castToButton "sensorOK"
                           sensorLabel  <-  builderGetObject builder castToLabel "sensorLabel"
                           getSensorButton <-  builderGetObject builder castToButton "getSensor"
                           monitorDialog  <-  builderGetObject builder castToDialog "monitorDialog"
                           monitorOKButton  <-  builderGetObject builder castToButton "monitorOK"                                                         
                           monitorCancelButton  <-  builderGetObject builder castToButton "monitorCancel"
                           monitorEntry <- builderGetObject builder castToEntry "monitorEntry"
                           startMonitorButton <- builderGetObject builder castToButton "startMonitor"
                           stopMonitorButton <- builderGetObject builder castToButton "stopMonitor"
                           imageWindow <- builderGetObject builder castToWindow "imageWindow"
                           showChartButton <- builderGetObject builder castToButton "showChart"
                           exitAppButton  <-  builderGetObject builder castToButton "exitApp"
                           
                           on exitAppButton buttonActivated (widgetDestroy mainWindow)
                           on mainWindow objectDestroy mainQuit
                           on relayOnButton buttonActivated $ do
                                                             info <- getHTTP "http://192.168.43.152/relay/1"
                                                             G.set sensorLabel [ labelText := "Przekaźnik włączony"]
                                                             widgetShowAll sensorDialog    

                           on relayOffButton buttonActivated $ do
                                                             info <- getHTTP "http://192.168.43.152/relay/0"
                                                             G.set sensorLabel [ labelText := "Przekaźnik wyłączony"]
                                                             widgetShowAll sensorDialog
 
                           on sensorOKButton buttonActivated (widgetHide sensorDialog)

                           on getSensorButton buttonActivated $ do
                                                                esp <- getEspData "http://192.168.43.152/sensor"
                                                                G.set sensorLabel [ labelText := "Aktualna temperatura: " ++ show (temperatureEspData ( fromJust esp)) ++ "\n Aktualna wilgotność: " ++ show (humidityEspData ( fromJust esp)) ]
                                                                widgetShowAll sensorDialog


                           on startMonitorButton buttonActivated (widgetShowAll monitorDialog)

                           on showChartButton buttonActivated $ do
                                                              makeChart
                                                              widgetShowAll imageWindow
                           
                           on monitorOKButton buttonActivated $ do
                                                                str <- entryGetText monitorEntry
                                                                let interval = read str :: Int
                                                                atomically $ modifyTVar (workingState thread) (*0)
                                                                atomically $ modifyTVar (workingState thread) (+1)
                                                                task <- forkIO $ forever $ do
                                                                                 x <- readTVarIO (workingState thread)
                                                                                 if x == 0
                                                                                    then return ()
                                                                                    else do 
                                                                                         saveSensorData
                                                                                         threadDelay (10^6 * interval)             
                                                                widgetHide monitorDialog
                                                                G.set sensorLabel [ labelText := "Rozpoczęto zbieranie danych"]
                                                                widgetShowAll sensorDialog
                           
                           on monitorCancelButton buttonActivated (widgetHide monitorDialog)
                            
                           on stopMonitorButton buttonActivated $ do
                                                             atomically $ modifyTVar (workingState thread) (*0)
                                                             G.set sensorLabel [ labelText := "Zatrzymano"]
                                                             widgetShowAll sensorDialog

                           widgetShowAll mainWindow
                           mainGUI
      
