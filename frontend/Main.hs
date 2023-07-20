module Main (main) where

import Reflex.Dom

import qualified NoGardenOnline

main :: IO ()
main = mainWidget $ NoGardenOnline.app
