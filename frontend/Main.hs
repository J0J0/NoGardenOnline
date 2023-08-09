{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Protolude hiding (State, state, lines)

import           Control.Monad.Trans.Maybe (MaybeT(MaybeT,runMaybeT))
import           Data.List (lookup)

import           Language.Javascript.JSaddle (JSVal, MonadJSM, (!), call, global, jsg, liftJSM, maybeNullOrUndefined)

import           GHCJS.DOM.Types (FromJSVal(fromJSVal, fromJSValListOf), JSString, MonadDOM)
import           GHCJS.DOM (currentWindow)
import           GHCJS.DOM.Window (getLocation)
import           GHCJS.DOM.Location (getSearch)
import           GHCJS.DOM.URLSearchParams (newURLSearchParams)
import qualified GHCJS.DOM.URLSearchParams as URLSearchParams (get)

import Reflex.Dom (divClass, el, holdDyn, leftmost, mainWidgetInElementById, text)

import qualified NoGardenOnline
import           NoGardenOnline (BoardSpec)

import Backports (hoistMaybe)


main :: IO ()
main = mainWidgetInElementById "app" $ mdo
    lvls <- fromMaybeT [] $ getLevels

    board_init <- fromMaybeT NoGardenOnline.welcomeStubBoard $
        asum [ getBoardFromUrlLevel lvls
             , getBoardFromUrlParam
             ]
    
    NoGardenOnline.app =<< holdDyn board_init level_chooser
    
    level_chooser <- divClass "LevelChooser" $ do
        el "p" $ text "Choose a level:"
        leftmost <$>
            mapM (NoGardenOnline.miniPreviewApp . snd) lvls
    
    return ()

  where
    fromMaybeT def = fmap (fromMaybe def) . runMaybeT

getUrlParam :: MonadDOM m => Text -> MaybeT m Text
getUrlParam name = (MaybeT currentWindow) >>= \ win -> do
        loc   <- getLocation win
        query <- getSearch @_ @JSString loc
        urlsp <- newURLSearchParams query
        MaybeT $ URLSearchParams.get @_ @Text @Text urlsp name

-- | Safely call a global function that may or may not exist.
callGlobal :: MonadJSM m => Text -> MaybeT m JSVal
callGlobal name =
    MaybeT (liftJSM (jsg name >>= maybeNullOrUndefined)) >>= \ f ->
        lift $ liftJSM $ call f global ()

getBoardFromUrlLevel :: forall m. MonadDOM m
                  => [(Text, BoardSpec)] -> MaybeT m BoardSpec
getBoardFromUrlLevel lvls = do
    id <- getUrlParam "level"
    hoistMaybe $ lookup id lvls

getBoardFromUrlParam :: MonadDOM m => MaybeT m BoardSpec
getBoardFromUrlParam = do
    size_str  <- getUrlParam "size"
    obs_str   <- getUrlParam "blocked"
    
    hoistMaybe $ do
        (cols,rows) <- readMaybe @(Int,Int) (toS size_str)
        obstacles   <- readMaybe @[(Int,Int)] (toS ("[" <> obs_str <> "]"))
        
        NoGardenOnline.defineBoard cols rows obstacles

getLevels :: forall m. MonadJSM m => MaybeT m [(Text, BoardSpec)]
getLevels =
    callGlobal "getNoGardenOnlineLevels" >>=
        fromJSValListOfT @JSVal >>= mapM jsobj_to_level
  where
    (!?) :: JSVal -> Text -> MaybeT m JSVal
    obj !? prop = MaybeT $ liftJSM $ (obj ! prop) >>= maybeNullOrUndefined
    fromJSValT :: forall a. (FromJSVal a) => JSVal -> MaybeT m a
    fromJSValT = MaybeT . liftJSM . fromJSVal
    fromJSValListOfT :: forall a. (FromJSVal a) => JSVal -> MaybeT m [a]
    fromJSValListOfT = MaybeT . liftJSM . fromJSValListOf
    to_pairs :: [[Int]] -> MaybeT m [(Int,Int)]
    to_pairs = hoistMaybe . mapM to_pair
    to_pair [x,y] = Just (x,y)
    to_pair _     = Nothing
    
    jsobj_to_level :: JSVal -> MaybeT m (Text, BoardSpec)
    jsobj_to_level o = do
        id        <- (o !? "id") >>= fromJSValT @Text
        cols      <- (o !? "cols") >>= fromJSValT @Int
        rows      <- (o !? "rows") >>= fromJSValT @Int
        obstacles <- (o !? "blocked") >>= fromJSValT @[[Int]] >>= to_pairs
        bspec     <- hoistMaybe $ NoGardenOnline.defineBoard cols rows obstacles
        return (id, bspec)
        
