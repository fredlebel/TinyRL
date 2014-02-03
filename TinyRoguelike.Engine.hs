{-# OPTIONS_GHC -XRankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module TinyRoguelike.Engine
( Floor (..)
, Wall (..)
, Item (..)
, NpcRace (..)
, NpcId
, Npc (Npc)
, Tile (Tile)
, mkTile
, Level
, LevelOp
, getFloor, getItem, getWall, getNpc
, setFloor, setItem, setWall, setNpc
, moveFloor, moveItem, moveWall, moveNpc
, GameState (GameStateCtor)
, Pos
, GameOp, runGameOp, execGameOp, evalGameOp
, NpcOp, runNpcOp
, runLevelOp
, npcWalk, whoAmI
, foldLevelM
, Direction (..)
, RandomProvider (..)
, MessageLogger (..)
, loadLevel
) where

import System.IO
import System.Random
import Data.Grid
import Data.Maybe
import qualified Data.Vector.Mutable as MV
import Control.Monad
--import Control.Monad.Operation
import Control.Monad.Identity
import Control.Applicative
import TinyRoguelike.LevelParser
import Text.ParserCombinators.Parsec

------------------------------------------------
-- Basic data structures for the map
------------------------------------------------

data Floor = Stone | Lava deriving (Eq, Ord, Enum, Read)
data Wall = Brick | Rubble deriving (Eq, Ord, Enum, Read)
data Item = Sword | Ectoplasm deriving (Eq, Ord, Enum, Read)
data NpcRace = Player | Rat | Goblin deriving (Eq, Ord, Enum, Read)

type NpcId = Int
newtype Npc = Npc (NpcRace, NpcId) deriving Eq

-- Show instances for debugging purposes only
instance Show Floor where
    show Stone = "."
    show Lava = "~"

instance Show Wall where
    show Brick = "#"
    show Rubble = "%"

instance Show Item where
    show Sword = "/"
    show Ectoplasm = "!"

instance Show Npc where
    show (Npc (Player, _)) = "@"
    show (Npc (Rat, _)) = "r"
    show (Npc (Goblin, _)) = "g"

data Tile = Tile
    { _floor :: Maybe Floor
    , _item :: Maybe Item
    , _wall :: Maybe Wall
    , _npc :: Maybe Npc
    }

instance Show Tile where
    show (Tile _ _ _ (Just o)) = show o
    show (Tile _ _ (Just o) _) = show o
    show (Tile _ (Just o) _ _) = show o
    show (Tile (Just o) _ _ _) = show o
    show _ = " "

mkTile :: Maybe Floor -> Maybe Item -> Maybe Wall -> Maybe Npc -> Tile
mkTile f i w n = Tile f i w n

------------------------------------------------
-- Level related functions
-- They are all in the GridOp monad because they
-- deal with a mutable vector.
------------------------------------------------

type Level = Grid Tile
type LevelOp = GridOp Tile

-- Base function to extract a property of a tile
getObject :: (Tile -> o) -> (Int, Int) -> LevelOp o
getObject fn pos = do
    t <- getM pos
    return $ fn t

-- Getters for various things found in a tile
getFloor = getObject _floor
getItem = getObject _item
getWall = getObject _wall
getNpc = getObject _npc

-- Setters for various things found in a tile
-- Not using a base function because fields are not first class.

setFloor :: (Int, Int) -> Maybe Floor -> LevelOp ()
setFloor pos o = do
    t <- getM pos
    setM pos $ t { _floor = o }

setItem :: (Int, Int) -> Maybe Item -> LevelOp ()
setItem pos o = do
    t <- getM pos
    setM pos $ t { _item = o }

setWall :: (Int, Int) -> Maybe Wall -> LevelOp ()
setWall pos o = do
    t <- getM pos
    setM pos $ t { _wall = o }

setNpc :: (Int, Int) -> Maybe Npc -> LevelOp ()
setNpc pos o = do
    t <- getM pos
    setM pos $ t { _npc = o }

-- Functions to move objects
--moveObject :: ((Int, Int) -> WorldOp (Maybe o)) -> ((Int, Int) -> (Maybe o) -> WorldOp ()) -> (Int, Int) -> (Int, Int) -> WorldOp ()
moveObject getter setter p1 p2 = do
    o1 <- getter p1
    o2 <- getter p2
    if (isJust o1 && isNothing o2)
        then do
            setter p1 Nothing
            setter p2 o1
            return True
        else return False

moveFloor = moveObject getFloor setFloor
moveItem = moveObject getItem setItem
moveWall = moveObject getWall setWall
moveNpc = moveObject getNpc setNpc

findOnLevel :: (Tile -> Bool) -> LevelOp (Maybe Pos)
findOnLevel fn = do
    pos <- foldGridM foldFn Nothing
    return pos
    where
        foldFn acc pos t = case fn t of
                                True -> Just pos
                                False -> acc


data GameState = GameStateCtor
    { _worldMap :: Grid Tile
    , _inventory :: [Item]
    , _messages :: [[String]]
    , _rnd :: StdGen
    }

type Pos = (Int, Int)
{-
data GameOpT
type GameOp = OperationST GameOpT (GameState, forall s. MV.MVector s Tile)
-}

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
        ((ret, game'), newGrid) = runGridOp (_worldMap game) (_gameOpFn op game)
        game'' = game' { _worldMap = newGrid }

evalGameOp game op = fst $ runGameOp game op
execGameOp game op = snd $ runGameOp game op

foldLevelM :: (acc -> Pos -> Tile -> acc) -> acc -> GameOp acc
foldLevelM fn acc = GameOpCtor $ \game -> do
    ret <- foldGridM fn acc
    return (ret, game)

runLevelOp :: LevelOp r -> GameOp r
runLevelOp op = GameOpCtor $ \game -> do
    ret <- op
    return (ret, game)

data NpcOp r = NpcOpCtor { _npcOpFn :: Pos -> GameOp (r, Pos) }

instance Functor NpcOp where
    fmap fn m = NpcOpCtor $ \pos -> do
        (ret, pos') <- _npcOpFn m pos
        return (fn ret, pos')

instance Monad NpcOp where
    return ret = NpcOpCtor $ \pos -> return (ret, pos)
    m >>= fn = NpcOpCtor $ \pos -> do
        (ret, pos') <- _npcOpFn m pos
        _npcOpFn (fn ret) pos'

runNpcOp :: Npc -> NpcOp r -> GameOp r
runNpcOp npc op = do
    Just pos <- runLevelOp $ findOnLevel (\t -> _npc t == Just npc)
    (r, pos') <- _npcOpFn op pos
    return r

data Direction = North | East | South | West
    deriving (Eq, Bounded, Enum)

offsetPos :: Direction -> Pos -> Pos
offsetPos dir (x, y) = case dir of
                            North -> (x, y-1)
                            South -> (x, y+1)
                            East  -> (x-1, y)
                            West  -> (x+1, y)

instance Random Direction where
    random g =
            case randomR (min, max) g of (r, g') -> (toEnum r, g')
            where
                min = fromEnum (minBound :: Direction)
                max = fromEnum (maxBound :: Direction)
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

npcWalk :: Direction -> NpcOp Bool
npcWalk dir = NpcOpCtor $ \oldPos -> do
    let newPos = offsetPos dir oldPos
    msg <- runLevelOp $ do
        onLevel <- containsM newPos
        if not onLevel
            then return $ Just "Npc tried to move off the map"
            else do
                wall <- getWall newPos
                npc <- getNpc newPos
                if  | isJust wall -> return $ Just "Npc bumped in wall"
                    | isJust npc  -> return $ Just "Npc bumped in another npc"
                    | otherwise   -> return Nothing
    ret <- case msg of
        Just str -> do
            logMessage (fromJust msg)
            return False
        otherwise -> do
            runLevelOp $ moveNpc oldPos newPos
            return True
    return (ret, if ret then newPos else oldPos)

whoAmI :: NpcOp (Npc, Pos)
whoAmI = NpcOpCtor $ \pos -> do
    Just npc <- runLevelOp $ getNpc pos
    return ((npc, pos), pos)

-- GameOp and NpcOp expose a random generator
class Monad m => RandomProvider m where
    getRandom :: (StdGen -> (ret, StdGen)) -> m ret

instance RandomProvider GameOp where
    getRandom fn = GameOpCtor $ \game -> do
        let (ret, g) = fn (_rnd game)
        return (ret, game { _rnd = g })

instance RandomProvider NpcOp where
    getRandom fn = NpcOpCtor $ \pos -> do
        ret <- getRandom fn
        return (ret, pos)


-------------------------------
class Monad m => MessageLogger m where
    -- Begin a new message frame.
    beginMessageFrame :: m ()
    -- Log a message in the current frame
    logMessage :: String -> m ()
    -- Getting messages
    getLastFrameMessages :: m [String]
    getAllMessages :: m [[String]]
    -- Clear any messages not in the current frame.
    clearOldMessageFrames :: m ()

instance MessageLogger GameOp where
    beginMessageFrame = GameOpCtor $ \game -> do
        let msgs = _messages game
        return ((), game { _messages = [] : msgs })
    logMessage msg = GameOpCtor $ \game -> do
        let (frame:frames) = _messages game
        return ((), game { _messages = (msg:frame):frames })
    getLastFrameMessages = GameOpCtor $ \game -> do
        let (frame:frames) = _messages game
        if null (_messages game)
            then return ([], game)
            else return (head . _messages $ game, game)
    getAllMessages = GameOpCtor $ \game -> do
        return (_messages game, game)
    clearOldMessageFrames = GameOpCtor $ \game -> do
        let (frame:frames) = _messages game
        return ((), game { _messages = [frame] })

instance MessageLogger NpcOp where
    beginMessageFrame     = NpcOpCtor $ \pos -> (, pos) <$> beginMessageFrame
    logMessage msg        = NpcOpCtor $ \pos -> (, pos) <$> logMessage msg
    getLastFrameMessages  = NpcOpCtor $ \pos -> (, pos) <$> getLastFrameMessages
    getAllMessages        = NpcOpCtor $ \pos -> (, pos) <$> getAllMessages
    clearOldMessageFrames = NpcOpCtor $ \pos -> (, pos) <$> clearOldMessageFrames

-- Level parsing and building


loadLevel :: Either String Level
loadLevel =
    case parseLevel levelData of
        Left err   -> Left err
        Right desc -> buildLevel desc
    where
        levelData = [str|
            # = (Stone, _, Brick, _)
            . = (Stone, _, _, _)
            @ = (Stone, _, _, Player)
            g = (Stone, _, _, Goblin)
            ~ = (Lava, _, _, _)
            (80x20)
            ################################################################################
            #...............#..............................................................#
            #.....g.........#..............................................................#
            #...............#....g.........................................................#
            #...............#..............................................................#
            #..............................................................................#
            #################...........~~~~...............................................#
            #....g..........#.........~~~~~~~~.............................................#
            #............@..#.........~~~~~~~~.............................................#
            #...............#.g.........~~~~~~~~...........................................#
            #...............#.............~~~~~~~~.........................................#
            ###############.#.................~~~~.........................................#
            #..................................~~..........................................#
            #..............................................................................#
            #...g..........................................................................#
            #..............................................................................#
            #..............................................................................#
            #..............................................................................#
            #..............................................................................#
            ################################################################################
        |]


buildLevel :: LevelDescription -> Either String Level
buildLevel desc = Right $ execGridOp emptyLevel $ do
    forM (zip [0..] (tiles desc)) $ \(i, ch) -> do
        -- TODO: Add failure support to GridOp
        let (Just (floorStr, itemStr, wallStr, avatarStr)) = lookup ch (table desc)
        let tile = mkTile
                (toObject floorStr)
                (toObject itemStr)
                (toObject wallStr)
                (toAvatar avatarStr i)
        setiM i tile
    where
        emptyLevel = mkGrid (dimension desc) (mkTile Nothing Nothing Nothing Nothing)
        toObject Nothing = Nothing
        toObject (Just str) = Just $ read str
        toAvatar Nothing _ = Nothing
        toAvatar (Just str) i = Just . Npc $ (read str, i)











