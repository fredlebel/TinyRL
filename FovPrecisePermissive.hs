{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module FovPrecisePermissive
( computeFov
, WallCheck
--, foldQuadrant
, quadrantTiles
) where

import Data.List
--import Control.Applicative hiding ((<*>))

type Pos = (Int, Int)

type WallCheck m = Pos -> m Bool

computeFov :: (Functor m, Monad m) => WallCheck m -> Pos -> m [Pos]
computeFov check startPos = do
    q1 <- doFovQuadrant check startPos (1, 1)
    q2 <- doFovQuadrant check startPos (1, -1)
    q3 <- doFovQuadrant check startPos (-1, 1)
    q4 <- doFovQuadrant check startPos (-1, -1)
    return $ nub ([startPos] ++ q1 ++ q2 ++ q3 ++ q4)
    --return (nub q1)

newtype Line = L (Pos, Pos) deriving (Show, Eq)
data View = V
    { steep :: Line
    , shallow :: Line
    , steepBumps :: [Pos]
    , shallowBumps :: [Pos]
    } deriving (Show, Eq)

startPoint (L (p, _)) = p
endPoint   (L (_, p)) = p

relativeSlope :: Line -> Pos -> Int
relativeSlope (L ((x1, y1), (x2, y2))) (x, y) =
    (y2 - y1) * (x2 - x) -
    (x2 - x1) * (y2 - y)

below           line p = relativeSlope line p < 0
above           line p = relativeSlope line p > 0
belowOrContains line p = relativeSlope line p <= 0
aboveOrContains line p = relativeSlope line p >= 0
contains        line p = relativeSlope line p == 0

viewValid :: View -> Bool
viewValid v = if
    | (shallow v) `above` (1, 0) -> False
    | (steep v) `below` (0, 1) -> False
    | not $ (shallow v) `contains` (startPoint (steep v)) -> True
    | not $ (shallow v) `contains` (endPoint   (steep v)) -> True
    -- | (shallow v) `contains` (0, 1) || (shallow v) `contains` (1,0) -> False
    -- | otherwise -> True
    | otherwise -> False


doFovQuadrant :: (Functor m, Monad m) => WallCheck m -> Pos -> (Int, Int) -> m [Pos]
doFovQuadrant check startPos posFlip = do
    let startView = V (L ((1, 0), (0, 999))) (L ((0, 1), (999, 0))) [] []
    visibleTiles <- walk (quadrantTiles (0,0)) [startView]
    return visibleTiles
    where
        walk _ [] = return []
        walk (tile:tiles) views = do
            let realPos = (tile <*> posFlip) <+> startPos
            isBlocking <- check realPos
            -- Check this tile's impact against all views
            let results = map (processTile tile isBlocking) views
            -- The new list of views
            let views' = filter viewValid . map checkForPreviousBumps . nub . concat . map snd $ results
            -- The tile is visible if it was visible in at least one of the views.
            let isVisible = or . map fst $ results
            -- Continue to the next tile
            rest <- walk tiles views'
            return $ if isVisible
                        then realPos : rest
                        else           rest
        walk _ _ = return []

-- Operator to offset a position
(<+>) :: Pos -> Pos -> Pos
(x1, y1) <+> (x2, y2) = (x1 + x2, y1 + y2)

-- Operator to multiply positions.
-- Used to map a relative quadrant coordinate into an absolute position.
(<*>) :: Pos -> Pos -> Pos
(x1, y1) <*> (x2, y2) = (x1 * x2, y1 * y2)

-- Operator to check if a position is within a view.
(>-<) :: Pos -> View -> Bool
pos >-< v = if
    | (shallow v) `belowOrContains` bottomLeft -> False
    | (steep v)   `aboveOrContains` topRight   -> False
    | otherwise                                -> True
    where
        bottomLeft = pos <+> (0, 1)
        topRight   = pos <+> (1, 0)

-- Operator to set the end point of a line.
(->>) :: Line -> Pos -> Line
(L (p1, _)) ->> p = L (p1, p)

-- Operator to set the start point of a line.
(<<-) :: Pos -> Line -> Line
p <<- (L (_, p2)) = L (p, p2)

checkForPreviousBumps :: View -> View
checkForPreviousBumps v = v {
    steep = foldr fnSteep (steep v) (shallowBumps v),
    shallow = foldr fnShallow (shallow v) (steepBumps v) }
    where
        fnShallow bump line = if line `below` bump then bump <<- line else line
        fnSteep bump line = if line `above` bump then bump <<- line else line

processShallowBump :: View -> Pos -> View
processShallowBump v pos = v {
    shallow = (shallow v) ->> pos,
    shallowBumps = pos : (shallowBumps v) }

processSteepBump :: View -> Pos -> View
processSteepBump v pos = v {
    steep = (steep v) ->> pos,
    steepBumps = pos : (steepBumps v) }

processTile :: Pos -> Bool -> View -> (Bool, [View])
processTile pos isBlocking v = if
    -- Tile doesn't even intersect the view
    | not $ pos >-< v -> (False, [v])
    -- Tile is not blocking LOS
    | not isBlocking -> (True, [v])
    -- Tile completely blocks the view
    | sh `below` topRight && st `above` bottomLeft -> (True, [])
    -- Tile splitting the view
    | sh `aboveOrContains` topRight && st `belowOrContains` bottomLeft ->
        (True, [v { shallow = sh ->> bottomLeft }, v { steep = st ->> topRight }])
    -- Shallow bump
    | sh `above` bottomLeft && sh `belowOrContains` topRight ->
        (True, [processShallowBump v bottomLeft]) -- [v { shallow = sh ->> bottomLeft, shallowBumps = bottomLeft : (shallowBumps v) }])
    -- Steep bump
    | st `below` topRight && st `aboveOrContains` bottomLeft ->
        (True, [processSteepBump v topRight]) -- [v { steep = st ->> topRight, steepBumps = topRight : (steepBumps v) }])
    | otherwise -> error $ "Pos : " ++ (show pos) ++ " View : " ++ (show v)
    where
        sh = shallow v
        st = steep v
        bottomLeft = pos <+> (0, 1)
        topRight   = pos <+> (1, 0)


--foldQuadrant :: (Pos -> acc -> acc) -> acc -> acc
--foldQuadrant fn acc = foldr fn acc $ quadrantTiles (0,0)

quadrantTiles (99, _) = []
quadrantTiles (x, 0) = (x, 0) : quadrantTiles (0,   x+1)
quadrantTiles (x, y) = (x, y) : quadrantTiles (x+1, y-1)

{-

6
37
148
@259


-}










