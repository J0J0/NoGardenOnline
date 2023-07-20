{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module NoGardenOnline (app) where

import Protolude hiding (State, state, lines)

import           Control.Monad.Fix        (MonadFix)
import qualified Data.Array.IArray  as A
import           Data.List                (lookup)
import qualified Data.Text          as T

import Reflex.Dom


gameDescription :: Text
gameDescription =
    "The goal is to fill the rectangular game field with horizontal and vertical lines,   \
    \such that each line starts and ends \"outside\" of the field. A started line extends \
    \in a straight manner until it hits a blocked tile or the outside. In the former      \
    \case, the line must be continued in either possible direction."


type M t m = (DomBuilder t m, PostBuild t m, MonadSample t m)

app :: (M t m, MonadFix m, MonadHold t m) => m ()
app = mdo
    el "h1" $ text "NoGardenOnline"
    el "p" $ text gameDescription
    state <- holdDyn testState $ leftmost $
        [ attachWith handle_click    (current state) (tileClick $ tileEvents $ ev)
        , attachWith handle_hover    (current state) (tileHover $ tileEvents $ ev)
        , attachWith handle_mouseout (current state) (mouseout ev)
        , attachWith handle_ctrls    (current state) ctrls
        ]
    ev <- boardW state
    ctrls <- buttonsW
    return ()
  where
    handle_click :: State -> Coord -> State
    handle_click  = const . addLine
    handle_hover :: State -> Coord -> State
    handle_hover = flip hover
    handle_mouseout :: State -> () -> State
    handle_mouseout state _ = state { previewLine = Nothing }
    handle_ctrls :: State -> ButtonAction -> State
    handle_ctrls state AbortLine  = cancelCurrentLine state
    handle_ctrls state ResetBoard = resetBoard state

data BoardEvents t = BoardEvents { tileEvents :: TileEvents t
                                 , mouseout   :: Event t () }

boardW :: M t m
       => Dynamic t State -> m (BoardEvents t)
boardW state = do
    (e, te) <- elAttr' "div" ("class" =: "board") $ do
        y_max <- sample $ (snd . boardMax' . board) <$> current state
        leftmostTileEvents <$> mapM (rowW state) [y_max, y_max-1 .. -1]
    return $ BoardEvents { tileEvents = te
                         , mouseout   = domEvent Mouseout e }

data ButtonAction = AbortLine | ResetBoard

buttonsW :: DomBuilder t m => m (Event t ButtonAction)
buttonsW = divClass "controls" $ fmap leftmost $ sequence $
    [ AbortLine  <$$ button "Abort line"
    , ResetBoard <$$ button "Reset board" ]
  where
    (<$$) :: (Functor m, Functor f) =>  a -> m (f b) -> m (f a)
    (<$$) = fmap . (<$)


rowW :: M t m
     => Dynamic t State -> Int -> m (TileEvents t)
rowW state y = divClass "row" $ do
    x_max <- sample $ (fst . boardMax' . board) <$> current state
    leftmostTileEvents <$> mapM (\ x -> tileW state (x,y)) [-1 .. x_max]

data TileEvents t = TileEvents { tileClick :: Event t Coord
                               , tileHover :: Event t Coord }

leftmostTileEvents :: Reflex t => [TileEvents t] -> TileEvents t
leftmostTileEvents = from_tuple . both leftmost . unzip . map to_tuple
  where
    to_tuple :: TileEvents t -> (Event t Coord, Event t Coord)
    to_tuple ev = (tileClick ev, tileHover ev)
    from_tuple :: (Event t Coord, Event t Coord) -> TileEvents t
    from_tuple (clickE,hoverE) = TileEvents clickE hoverE

tileW :: M t m
      => Dynamic t State
      -> Coord -> m (TileEvents t)
tileW state coord = do
    (e, _) <- elDynAttr' "div" ((\ s -> "class" =: ("tile" <> s)) <$> dyn_class) blank
    return $ (uncurry TileEvents) $ both (coord <$) (domEvent Click e, domEvent Mouseover e)
  where
    dyn_class =
        ffor state $ \ (State {board = b, previewLine = prev, currentLine = cur}) ->
            case b A.! coord of
                EmptyTile     ->
                    display_prev_line prev <> continue_hint prev cur b
                ObstacleTile  ->
                    " obstacle"
                InvisibleTile ->
                    " invisible" <> start_hint prev cur b <> continue_hint prev cur b
                lt@(LineTile _ _) -> -- FIXME: buggy for equal line tile dirs
                    " " <> line_tile_to_classname lt <> highlight_current cur <> highlight_solved cur b
    
    display_prev_line prev = fromMaybe "" $ do
        l  <- prev
        lt <- lookup coord (lineToTiles l)
        return $ " hint" <> line_tile_to_classname lt
    
    continue_hint Nothing (l:_) b = case lastMay (lineSegments l) of
        Nothing  -> ""  -- Note: should never happen
        Just seg -> case lookup coord (filter (legal_move b) (cont_coord seg)) of
            Nothing    -> ""
            Just North -> " arrowup"
            Just East  -> " arrowright"
            Just South -> " arrowdown"
            Just West  -> " arrowleft"
    continue_hint _ _ _ = ""
    
    start_hint Nothing [] b = case lookup coord (boundaryCoordsFree b) of
        Nothing    -> ""
        Just North -> " arrowdown"
        Just East  -> " arrowleft"
        Just South -> " arrowup"
        Just West  -> " arrowright"
    start_hint _ _ _ = ""
    
    highlight_current ls = case joinRevLines ls of
        Nothing -> ""
        Just l  -> if coord `elem` lineCoordinates l
                   then " current"
                   else ""
    
    highlight_solved cur b = case (cur, any isEmptyTile b) of
        ([], False) -> " solved"
        (_, _)      -> ""
    
    line_tile_to_classname (LineTile d1 d2) =
        T.toLower $ T.concat $ map show $ sort [d1,d2]
    
    cont_coord (Segment (x,y) dir) = case orientationOf dir of
        Vertical   -> [((x-1,y), West),  ((x+1,y), East)]
        Horizontal -> [((x,y-1), South), ((x,y+1), North)]
    
    legal_move b (c@(x,y), _)
        | x < 0 || y < 0 || x > mx || y > my  = True
        | isEmptyTile (b A.! c)        = True
        | otherwise                           = False
      where
        (mx,my) = boardMax b


-----------------------------------------------------------------
-----------------------------------------------------------------
-----------------------------------------------------------------

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.).(.)

both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

foldrMay1 :: (a -> a -> Maybe a) -> [a] -> Maybe a
foldrMay1 _ []     = Nothing
foldrMay1 _ [x]    = return x
foldrMay1 f (x:xs) = f x =<< foldrMay1 f xs


testState = State b Nothing []
  where
    (bx,by) = (6,5)
    bounds  = ((-1,-1),(bx+1,by+1))
    b' = A.listArray bounds $ foreach (A.range bounds) $ \ (x,y) ->
        if x < 0 || x > bx || y < 0 || y > by
        then InvisibleTile
        else EmptyTile
    b = b' A.// [((1,1), ObstacleTile), ((3,2), ObstacleTile)]
    --ls = [l1]
    --l1 = Line (Segment (6,0) West) [Straight, Straight, TurnRight, Straight, TurnLeft, TurnRight]
    --us = map (\ t -> (t, UsedTile)) [(6,0),(5,0),(4,0),(4,1),(4,2),(3,2)]
    --b  = b1 A.// us


data Tile = EmptyTile
          | LineTile CardinalDir CardinalDir
          | ObstacleTile
          | InvisibleTile
          
type Coord = (Int,Int)
type Board = A.Array Coord Tile

data LineDir = Straight | TurnLeft | TurnRight
data CardinalDir = North | East | South | West
    deriving (Enum, Bounded, Eq, Ord, Show)
data LineSegment = Segment Coord CardinalDir
data Line = Line { startSeg    :: LineSegment
                 , directions  :: [LineDir]
                 }
          
data State = State { board :: Board
                   , previewLine :: Maybe Line
                   , currentLine :: [Line]
                   }

data Orientation = Horizontal | Vertical



dirToAngle :: CardinalDir -> Float
dirToAngle = fromIntegral . (90*) . fromEnum

invertDir :: CardinalDir -> CardinalDir
invertDir North = South
invertDir East  = West
invertDir South = North
invertDir West  = East


isEmptyTile :: Tile -> Bool
isEmptyTile EmptyTile = True
isEmptyTile _         = False

boardMax', boardMax :: Board -> (Int,Int)
boardMax' = snd . A.bounds
boardMax  = both (subtract 1) . boardMax'

boardDims', boardDims :: Board -> (Int,Int)
boardDims' = both (+1) . snd . A.bounds
boardDims  = both (subtract 1) . boardDims'


boundaryCoords, boundaryCoordsFree :: Board -> [(Coord, CardinalDir)]
boundaryCoords = boundaryCoordsFilter (const True)
boundaryCoordsFree b = boundaryCoordsFilter (isEmptyTile . (b A.!)) b

-- filter based on nearest coordinate inside the board
boundaryCoordsFilter :: (Coord -> Bool) -> Board -> [(Coord, CardinalDir)]
boundaryCoordsFilter f b = bdy_n ++ bdy_e ++ bdy_s ++ bdy_w
  where
    (mx,my) = boardMax b
    bdy_w   = [((-1,  k), West)  | k <- [0..my], f (0, k)]
    bdy_e   = [((mx+1,k), East)  | k <- [0..my], f (mx,k)]
    bdy_s   = [((k,  -1), South) | k <- [0..mx], f (k, 0)]
    bdy_n   = [((k,my+1), North) | k <- [0..mx], f (k,my)]

startCoord :: Line -> Coord
startCoord l = case startSeg l of Segment c _ -> c

startDir :: Line -> CardinalDir
startDir l = case startSeg l of Segment _ dir -> dir

orientationOf :: CardinalDir -> Orientation
orientationOf North = Vertical
orientationOf East  = Horizontal
orientationOf South = Vertical
orientationOf West  = Horizontal

emptyLine :: Coord -> CardinalDir -> Line
emptyLine c dir = Line { startSeg   = Segment c dir
                       , directions = [] }

guardEmptyLine :: Line -> Maybe Line
guardEmptyLine l = case directions l of
    [] -> Nothing
    _  -> Just l

isEmptyLine :: Line -> Bool
isEmptyLine = isNothing . guardEmptyLine

--------------------

hover :: Coord -> State -> State
hover (x,y) st = st { previewLine = (if null (currentLine st)
                          then startLine mx my x y
                          else continueLine st x y)
                          <&> extendLine st
--                          >>= guardEmptyLine
                    }
  where
      (mx,my) = boardMax (board st)

startLine :: Int -> Int -> Int -> Int -> Maybe Line
startLine mx my x y = case (x,mx-x,y,my-y) of
    (-1,_,_,_) | withinY -> mk_line (0,y)  East
    (_,-1,_,_) | withinY -> mk_line (mx,y) West
    (_,_,-1,_) | withinX -> mk_line (x,0)  North
    (_,_,_,-1) | withinX -> mk_line (x,my) South
    _                    -> Nothing
  where
    mk_line = Just .: emptyLine
    withinX = 0 <= x && x <= mx
    withinY = 0 <= y && y <= my

continueLine :: State -> Int -> Int -> Maybe Line
continueLine st x y =
    listToMaybe (currentLine st)
    >>= (lastMay . lineSegments)
    >>= \ (Segment (lx,ly) dir) ->
        case (x-lx, y-ly, orientationOf dir) of
            (-1, 0, Vertical)   -> mk_line (lx-1,ly) West
            ( 1, 0, Vertical)   -> mk_line (lx+1,ly) East
            ( 0, 1, Horizontal) -> mk_line (lx,ly+1) North
            ( 0,-1, Horizontal) -> mk_line (lx,ly-1) South
            _                   -> Nothing
  where
    mk_line = Just .: emptyLine

extendLine :: State -> Line -> Line
extendLine st l =
    l { directions = directions l ++ extendStraight st seg }
  where
    seg = lastDef (startSeg l) $ lineSegments l


extendStraight :: State -> LineSegment -> [LineDir]
extendStraight st (Segment c dir) = Straight <$ takeWhile isEmptyTile tiles
  where
    tiles    = (board st A.!) <$> take max_num (iterate step c)
    (x,y)    = c
    (xd, yd) = boardDims (board st)
    (max_num, step) = case dir of
        North -> (yd-y, second (+1)        )
        East  -> (xd-x, first  (+1)        )
        South -> (y+1,  second (subtract 1))
        West  -> (x+1,  first  (subtract 1))

--------------------

addLine :: State -> State
addLine st = case previewLine st of
    Nothing -> st
    Just l  -> if lineComplete st l
               then State { board = maybe (board st) (board st A.//) (lineToTiles <$> joinRevLines (l : currentLine st))
                          , currentLine = []
                          , previewLine = Nothing
                          }
               else if isEmptyLine l then st
                    else let new_current_line = l : currentLine st
                         in st { board = maybe (board st) (board st A.//) (lineToTiles <$> joinRevLines new_current_line)
                               , currentLine = new_current_line
                               , previewLine = Nothing
                               }

lineComplete :: State -> Line -> Bool
lineComplete st l =
    case lastDef (startSeg l, 1) (lineSegments l `zip` repeat 0) of
        (Segment (x,y) dir, t) ->
            case (x+t, mx-x+t, y+t, my-y+t, dir) of
                (0,_,_,_, West)  -> True
                (_,0,_,_, East)  -> True
                (_,_,0,_, South) -> True
                (_,_,_,0, North) -> True
                _                -> False
  where
    (mx,my) = boardMax (board st)

joinLines :: [Line] -> Maybe Line
joinLines = foldrMay1 joinTwoLines

joinRevLines :: [Line] -> Maybe Line
joinRevLines = joinLines . reverse

-- first line should be non-empty
joinTwoLines :: Line -> Line -> Maybe Line
joinTwoLines l1 l2 = do
    Segment (x1,y1) dir1 <- lastMay (lineSegments l1)
    let Segment (x2,y2) dir2 = startSeg l2
    joinDir <- case (dir1, x2-x1, y2-y1, dir2) of
        (North,  0, 1, North) -> Just Straight
        (North,  1, 0, East)  -> Just TurnRight
        (North, -1, 0, West)  -> Just TurnLeft
        --
        (East,   1, 0, East)  -> Just Straight
        (East,   0, 1, North) -> Just TurnLeft
        (East,   0,-1, South) -> Just TurnRight
        --
        (South,  0,-1, South) -> Just Straight
        (South,  1, 0, East)  -> Just TurnLeft
        (South, -1, 0, West)  -> Just TurnRight
        --
        (West,  -1, 0, West)  -> Just Straight
        (West,   0, 1, North) -> Just TurnRight
        (West,   0,-1, South) -> Just TurnLeft
        --
        _ -> Nothing
    (d1',_) <- unsnoc (directions l1)
    return $ l1 { directions = d1' ++ [joinDir] ++ directions l2 }

--------------------

cancelCurrentLine :: State -> State
cancelCurrentLine st = case currentLine st of
    [] -> st
    ls -> st { board = board st A.// gen_updates ls
             , previewLine = Nothing
             , currentLine = []
             }
  where
    gen_updates = map (\ t -> (t, EmptyTile)) . concatMap lineCoordinates

resetBoard :: State -> State
resetBoard st = State { board = reset_tile `A.amap` board st
                      , previewLine = Nothing
                      , currentLine = []
                      }
  where
    reset_tile (LineTile _ _) = EmptyTile
    reset_tile tile           = tile

--------------------

lineCoordinates :: Line -> [Coord]
lineCoordinates = map (\ (Segment coord _) -> coord) . lineSegments

lineSegments :: Line -> [LineSegment]
lineSegments = lineSegments' False
lineSegmentsExt :: Line -> [LineSegment]
lineSegmentsExt = lineSegments' True

lineSegments' :: Bool -> Line -> [LineSegment]
lineSegments' with_end l = case directions l of
    []     -> if with_end then [startSeg l] else []
    (d:ds) -> startSeg l : lineSegments' with_end l'
      where
        l' = Line { startSeg = Segment c' o'
                  , directions = ds
                  }
        c' = step (startCoord l)
        (step, o') = case d of
            Straight  -> (caseDir up right down left, startDir l)
            TurnLeft  -> (caseDir left up right down, turnL     )
            TurnRight -> (caseDir right down left up, turnR     )
        caseDir n e s w =
            case startDir l of { North -> n; East  -> e;
                                 South -> s; West  -> w }
        up    = second (+1)
        right = first  (+1)
        down  = second (subtract 1)
        left  = first  (subtract 1)
        turnL = caseDir West North East South
        turnR = caseDir East South West North

lineTiles :: [LineSegment] -> [(Coord, Tile)]
lineTiles segs = map (uncurry go) (segs `zip` tailDef segs segs)
  where
    go (Segment c1 dir1) (Segment _ dir2) = (c1, LineTile (invertDir dir1) dir2)

lineToTiles :: Line -> [(Coord, Tile)]
lineToTiles = lineTiles . lineSegmentsExt