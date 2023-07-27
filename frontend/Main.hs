{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Protolude hiding (State, state, lines)

import           GHCJS.DOM.Types (JSString, MonadDOM)
import           GHCJS.DOM (currentWindow)
import           GHCJS.DOM.Window (getLocation)
import           GHCJS.DOM.Location (getSearch)
import           GHCJS.DOM.URLSearchParams (newURLSearchParams)
import qualified GHCJS.DOM.URLSearchParams as URLSearchParams (get)

import Reflex.Dom (constDyn, mainWidgetInElementById)

import qualified NoGardenOnline


getUrlParam :: MonadDOM m => Text -> m (Maybe Text)
getUrlParam name = currentWindow >>= \case
    Nothing  -> return Nothing
    Just win -> do
        loc   <- getLocation win
        query <- getSearch @_ @JSString loc
        urlsp <- newURLSearchParams query
        URLSearchParams.get @_ @Text @Text urlsp name

main :: IO ()
main = mainWidgetInElementById "app" $ do
    may_board_size_str <- getUrlParam "size"
    may_board_obstacles_str <- getUrlParam "blocked"
    
    let board = fromMaybe NoGardenOnline.welcomeStubBoard $ do
            size_str <- may_board_size_str <&> toS
            obs_str  <- may_board_obstacles_str <&> toS
            
            (cols,rows) <- readMaybe @(Int,Int) size_str
            obstacles   <- readMaybe @[(Int,Int)] ("[" <> obs_str <> "]")
            
            NoGardenOnline.defineBoard cols rows obstacles
    
    NoGardenOnline.app (constDyn board)
