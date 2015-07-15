{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           Snap.Snaplet

import App(App, appInit)

main :: IO ()
main = serveSnaplet defaultConfig appInit

