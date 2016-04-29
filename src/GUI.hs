{-# LANGUAGE TemplateHaskell #-}
module GUI where

import Graphics.UI.Gtk as G
import ChartGenerator
import Control.Concurrent
import Data.Maybe
import EspHandler
import Sensor
import Control.Monad
import Control.Concurrent.STM
import Control.Monad.IO.Class

data State = State {working :: TVar Int}

workingState :: State -> TVar Int
workingState (State _working) = _working

data IP = IP {text :: TVar String}

addrIP :: IP -> TVar String
addrIP (IP _addr) = _addr



showMainWindow :: FilePath -> IO ()
showMainWindow gladepath = do
                           value <- newTVarIO 0 :: IO (TVar Int)
                           address <- newTVarIO "192.168.43.152" :: IO (TVar String)
			   let thread = State value
                           let addr = IP address
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
                           configDialog  <-  builderGetObject builder castToDialog "configDialog"
                           configOKButton  <-  builderGetObject builder castToButton "configOK"                                                         
                           configCancelButton  <-  builderGetObject builder castToButton "configCancel"
                           configEntry <- builderGetObject builder castToEntry "configEntry"
                           configButton <- builderGetObject builder castToButton "configButton"
                           exitAppButton  <-  builderGetObject builder castToButton "exitApp"
                           
                           on exitAppButton buttonActivated (widgetDestroy mainWindow)
                           on mainWindow objectDestroy mainQuit
                           on relayOnButton buttonActivated $ do
                                                             ip <- readTVarIO (addrIP addr)
                                                             let relayOnAddr = "http://"++ ip ++"/relay/1"
                                                             info <- getHTTP relayOnAddr
                                                             G.set sensorLabel [ labelText := "Przekaźnik włączony"]
                                                             widgetShowAll sensorDialog    

                           on relayOffButton buttonActivated $ do
                                                             ip <- readTVarIO (addrIP addr)
                                                             let relayOffAddr = "http://"++ ip ++"/relay/0"
                                                             info <- getHTTP relayOffAddr
                                                             G.set sensorLabel [ labelText := "Przekaźnik wyłączony"]
                                                             widgetShowAll sensorDialog
 
                           on sensorOKButton buttonActivated (widgetHide sensorDialog)

                           on getSensorButton buttonActivated $ do
                                                                ip <- readTVarIO (addrIP addr)
                                                                let sensorAddr = "http://"++ ip ++"/sensor"
                                                                esp <- getEspData sensorAddr
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
                                                                                         ip <- readTVarIO (addrIP addr)
                                                                                         let sensorAddr = "http://"++ ip ++"/sensor"
                                                                                         saveSensorData sensorAddr
                                                                                         let time = 10^6 * interval :: Int
                                                                                         threadDelay (time)             
                                                                widgetHide monitorDialog
                                                                G.set sensorLabel [ labelText := "Rozpoczęto zbieranie danych"]
                                                                widgetShowAll sensorDialog
                           
                           on monitorCancelButton buttonActivated (widgetHide monitorDialog)
                            
                           on stopMonitorButton buttonActivated $ do
                                                             atomically $ modifyTVar (workingState thread) (*0)
                                                             G.set sensorLabel [ labelText := "Zatrzymano"]
                                                             widgetShowAll sensorDialog

                           on configButton buttonActivated (widgetShowAll configDialog)
                           on configOKButton buttonActivated $ do
                                                                str <- entryGetText configEntry
                                                                let address = str
                                                                atomically $ writeTVar (addrIP addr) address      
                                                                widgetHide configDialog
                                                                G.set sensorLabel [ labelText := "Ustawiono IP"]
                                                                widgetShowAll sensorDialog
                           
                           on configCancelButton buttonActivated (widgetHide configDialog) 
                           
                           widgetShowAll mainWindow
                           mainGUI
      
