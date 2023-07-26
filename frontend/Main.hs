{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Reflex.Dom (mainWidgetInElementById)
import qualified NoGardenOnline

main :: IO ()
main = mainWidgetInElementById "app" $ NoGardenOnline.app
