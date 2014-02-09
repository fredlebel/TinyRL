{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module TinyRoguelike.Engine.GameOp
( GameOp, runGameOp, execGameOp, evalGameOp
, GameState
, mkGame
, runLevelOp
, getFloor, getItem, getWall, getNpc, getTile
, setFloor, setItem, setWall, setNpc
, moveFloor, moveItem, moveWall, moveNpc
, foldLevelM, foldNpcs
, findOnLevel, findNpc, findPlayer
, getInventory, addToInventory
, isTileBlocked
, onLevel
, processFov, viewedTiles
) where

import System.Random
import Data.Maybe
import Data.Grid
import Data.List
import Control.Applicative
import TinyRoguelike.Engine
import FovPrecisePermissive


data GameState = GameStateCtor
    { _worldMap :: Level
    , _memory :: [Pos]
    , _inventory :: [Item]
    , _messages :: [[String]]
    , _rnd :: StdGen
    }

mkGame = GameStateCtor

data GameOp r = GameOpCtor { _gameOpFn :: GameState -> LevelOp (r, GameState) }

instance Functor GameOp where
    fmap fn m = GameOpCtor $ \game -> do
        (ret, game') <- _gameOpFn m game
        return (fn ret, game')

instance Monad GameOp where
    return ret = GameOpCtor $ \game -> return (ret, game)
    m >>= fn = GameOpCtor $ \game -> do
        (ret, game') <- _gameOpFn m game
        _gameOpFn (fn ret) game'

runGameOp :: GameState -> GameOp r -> (r, GameState)
runGameOp game op = (ret, game'')
    where
        -- TODO: Error propagation.
        (Right (ret, game'), newGrid) = runGridOp (_worldMap game) (_gameOpFn op game)
        game'' = game' { _worldMap = newGrid }

evalGameOp game op = fst $ runGameOp game op
execGameOp game op = snd $ runGameOp game op

runLevelOp :: LevelOp r -> GameOp r
runLevelOp op = GameOpCtor $ \game -> do
    ret <- op
    return (ret, game)

instance RandomProvider GameOp where
    getRandom fn = GameOpCtor $ \game -> do
        let (ret, g) = fn (_rnd game)
        return (ret, game { _rnd = g })

instance MessageLogger GameOp where
    beginMessageFrame = GameOpCtor $ \game -> do
        let msgs = _messages game
        return ((), game { _messages = [] : msgs })
    logMessage msg = GameOpCtor $ \game -> do
        let (frame:frames) = _messages game
        return ((), game { _messages = (msg:frame):frames })
    getLastFrameMessages = GameOpCtor $ \game -> return $
        if null (_messages game)
            then ([], game)
            else (head . _messages $ game, game)
    getAllMessages = GameOpCtor $ \game ->
        return (_messages game, game)
    clearOldMessageFrames = GameOpCtor $ \game -> do
        let frame = head . _messages $ game
        return ((), game { _messages = [frame] })

-- Base functions

-- Base function to extract a property of a tile
getObject :: (Tile -> o) -> (Int, Int) -> GameOp o
getObject fn pos = runLevelOp $ do
    t <- getM pos
    return $ fn t

-- Getters for various things found in a tile
getFloor = getObject _floor
getItem = getObject _item
getWall = getObject _wall
getNpc = getObject _npc
getTile = getObject id

-- Setters for various things found in a tile
-- Not using a base function because fields are not first class.

setFloor pos o = runLevelOp $ do
    t <- getM pos
    setM pos $ t { _floor = o }

setItem pos o = runLevelOp $ do
    t <- getM pos
    setM pos $ t { _item = o }

setWall pos o = runLevelOp $ do
    t <- getM pos
    setM pos $ t { _wall = o }

setNpc pos o = runLevelOp $ do
    t <- getM pos
    setM pos $ t { _npc = o }

-- Functions to move objects
moveObject getter setter p1 p2 = do
    o1 <- getter p1
    o2 <- getter p2
    if isJust o1 && isNothing o2
        then do
            _ <- setter p1 Nothing
            _ <- setter p2 o1
            return True
        else return False

moveFloor = moveObject getFloor setFloor
moveItem = moveObject getItem setItem
moveWall = moveObject getWall setWall
moveNpc = moveObject getNpc setNpc

onLevel :: Pos -> GameOp Bool
onLevel pos = runLevelOp $ containsM pos

foldLevelM :: (acc -> Pos -> Tile -> acc) -> acc -> GameOp acc
foldLevelM fn acc = GameOpCtor $ \game -> do
    ret <- foldGridM fn acc
    return (ret, game)

foldNpcs :: (acc -> Pos -> Npc -> acc) -> acc -> GameOp acc
foldNpcs fn = foldLevelM foldFn
    where
        foldFn acc pos (Tile _ _ _ (Just npc)) = fn acc pos npc
        foldFn acc _ _ = acc

findOnLevel :: (Tile -> Bool) -> GameOp (Maybe Pos)
findOnLevel fn = runLevelOp $ foldGridM foldFn Nothing
    where
        foldFn acc pos t = if fn t then Just pos else acc

findNpc :: (Npc -> Bool) -> GameOp (Maybe Pos)
findNpc fn = findOnLevel (\t -> match . _npc $ t)
    where
        match Nothing = False
        match (Just npc) = fn npc

findPlayer :: GameOp Pos
findPlayer = fromJust <$> findNpc match
    where
        match (Npc (Player, _)) = True
        match _ = False

getInventory = GameOpCtor $ \game -> do
    return (_inventory game, game)

addToInventory item = GameOpCtor $ \game -> do
    return ((), game { _inventory = (_inventory game) ++ [item] })

isTileBlocked pos = do
    blockingWall <- (maybe False blocksMove) <$> getWall pos
    blockingNpc <- isJust <$> getNpc pos
    return $ blockingWall || blockingNpc

rememberTiles :: [Pos] -> GameOp ()
rememberTiles tiles = GameOpCtor $ \game -> return ((), game { _memory = newMemory game })
    where
        newMemory game = nub $ (_memory game) ++ tiles

viewedTiles :: GameOp [Pos]
viewedTiles = GameOpCtor $ \game -> return (_memory game, game)

processFov :: GameOp ()
processFov = do
    pos <- findPlayer
    visibleTiles <- computeFov check pos
    rememberTiles visibleTiles
    where
        check pos = do
            outOfBounds <- not <$> onLevel pos
            if outOfBounds
                then return True
                else maybe False blocksMove <$> getWall pos



