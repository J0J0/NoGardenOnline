{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Protolude hiding (State, state, lines)

import           Control.Monad.Trans.Maybe (MaybeT(MaybeT,runMaybeT))

import           Language.Javascript.JSaddle (JSVal, MonadJSM, (!), call, global, jsg, liftJSM, maybeNullOrUndefined)

import           GHCJS.DOM.Types (FromJSVal(fromJSVal), JSString, MonadDOM)
import           GHCJS.DOM (currentWindow)
import           GHCJS.DOM.Window (getLocation)
import           GHCJS.DOM.Location (getSearch)
import           GHCJS.DOM.URLSearchParams (newURLSearchParams)
import qualified GHCJS.DOM.URLSearchParams as URLSearchParams (get)

import Reflex.Dom (constDyn, mainWidgetInElementById)

import qualified NoGardenOnline
import           NoGardenOnline (BoardSpec)

-- | Convert a 'Maybe' computation to 'MaybeT'.
-- Taken from @transformers ^>= 0.6@.
hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure


main :: IO ()
main = mainWidgetInElementById "app" $ do
    board <- fmap (fromMaybe NoGardenOnline.welcomeStubBoard) $ runMaybeT $
        asum [ getBoardFromLevel
             , getBoardFromUrl
             ]
    NoGardenOnline.app (constDyn board)

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

getBoardFromLevel :: forall m. MonadDOM m => MaybeT m BoardSpec
getBoardFromLevel = do
    name     <- getUrlParam "level"
    lvls_js  <- callGlobal "getNoGardenOnlineLevels"
    lvl_js   <- lvls_js !? name
    cols     <- (lvl_js !? "cols") >>= fromJSValT @Int
    rows     <- (lvl_js !? "rows") >>= fromJSValT @Int
    obstacles <- (lvl_js !? "blocked") >>= fromJSValT @[[Int]] >>= to_pairs
    hoistMaybe $ NoGardenOnline.defineBoard cols rows obstacles
  where
    (!?) :: JSVal -> Text -> MaybeT m JSVal
    obj !? prop = MaybeT $ liftJSM $ (obj ! prop) >>= maybeNullOrUndefined
    fromJSValT :: forall a. (FromJSVal a) => JSVal -> MaybeT m a
    fromJSValT = MaybeT . liftJSM . fromJSVal
    to_pairs :: [[Int]] -> MaybeT m [(Int,Int)]
    to_pairs = hoistMaybe . mapM to_pair
    to_pair [x,y] = Just (x,y)
    to_pair _     = Nothing

getBoardFromUrl :: MonadDOM m => MaybeT m BoardSpec
getBoardFromUrl = do
    size_str  <- getUrlParam "size"
    obs_str   <- getUrlParam "blocked"
    
    hoistMaybe $ do
        (cols,rows) <- readMaybe @(Int,Int) (toS size_str)
        obstacles   <- readMaybe @[(Int,Int)] (toS ("[" <> obs_str <> "]"))
        
        NoGardenOnline.defineBoard cols rows obstacles
