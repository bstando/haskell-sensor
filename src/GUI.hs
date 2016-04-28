module GUI where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

import Control.Concurrent



showMainWindow gladepath = do
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
                           on getSensorButton buttonActivated (widgetShowAll sensorDialog)
                           
                           startMonitorButton <- builderGetObject builder castToButton "startMonitor"
                           stopMonitorButton <- builderGetObject builder castToButton "stopMonitor"
                           showChartButton <- builderGetObject builder castToButton "showChart"
                           exitAppButton  <-  builderGetObject builder castToButton "exitApp"
                           on exitAppButton buttonActivated (widgetDestroy mainWindow)
                           on mainWindow objectDestroy mainQuit
                          
                           monitorDialog  <-  builderGetObject builder castToDialog "monitorDialog"
                           monitorOKButton  <-  builderGetObject builder castToButton "monitorOK"
                           monitorCancelButton  <-  builderGetObject builder castToButton "monitorCancel"
                           monitorEntry <- builderGetObject builder castToEntry "monitorEntry"
                           widgetShowAll mainWindow
                           mainGUI
      
