{-# LANGUAGE OverloadedStrings #-}

module Frontend.NoGardenOnline (app) where

import Reflex.Dom

data TileType = FreeTile | ObstacleTile | InvisibleTile


app :: DomBuilder t m => m ()
app = do
    el "h1" $ text "NoGardenOnline"
    board 8 5
    return ()

board :: DomBuilder t m
      => Int -> Int -> m ()
board col_count row_count = do
    divClass "board" $ do
        row col_count InvisibleTile
        mapM_ (row col_count) (replicate row_count FreeTile)
        row col_count InvisibleTile

row :: DomBuilder t m
    => Int -> TileType -> m ()
row size tile_type = divClass "row" $
    mapM_ tile $ [InvisibleTile] <> replicate size tile_type <> [InvisibleTile]
 
tile :: DomBuilder t m
     => TileType -> m ()
tile tile_type = do
    divClass ("tile" <> extra_class) blank
    return ()
  where
    extra_class = case tile_type of
        FreeTile      -> " free"
        ObstacleTile  -> " obstacle"
        InvisibleTile -> ""
