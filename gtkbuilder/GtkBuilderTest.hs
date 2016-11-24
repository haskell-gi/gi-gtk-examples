{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Maybe (fromJust)
import qualified Data.Text as T (unpack)
import Data.GI.Base.ManagedPtr (unsafeCastTo)
import Data.GI.Base.GError (gerrorMessage, GError(..))
import Control.Exception (catch)
import qualified GI.Gtk as Gtk (main, init)
import GI.Gtk
       (widgetShowAll, mainQuit, onWidgetDestroy, onButtonClicked, Button(..),
        Window(..), builderGetObject, builderAddFromFile, builderNew)

main = (do
    Gtk.init Nothing

    -- Create the builder, and load the UI file
    builder <- builderNew
    builderAddFromFile builder "simple.ui"

    -- Retrieve some objects from the UI
    window <- builderGetObject builder "window1" >>= unsafeCastTo Window . fromJust
    button <- builderGetObject builder "button1" >>= unsafeCastTo Button . fromJust

    -- Basic user interation
    onButtonClicked button $ putStrLn "button pressed!"
    onWidgetDestroy window mainQuit

    -- Display the window
    widgetShowAll window
    Gtk.main)
  `catch` (\(e::GError) -> gerrorMessage e >>= putStrLn . T.unpack)
