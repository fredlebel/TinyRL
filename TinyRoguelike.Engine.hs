{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module TinyRoguelike.Engine
( Floor (..)
, Wall (..)
, Item (..)
, NpcRace (..)
, NpcId
, Npc (Npc)
, Tile (..)
, mkTile
, findNpc, findPlayer
, Level
, LevelOp
, getFloor, getItem, getWall, getNpc
, setFloor, setItem, setWall, setNpc
, moveFloor, moveItem, moveWall, moveNpc
, Pos
, Direction (..)
, RandomProvider (..)
, MessageLogger (..)
, buildLevel
, level
) where

import System.IO
import System.Random
import Data.Grid
import Data.Maybe
import Data.List
import Data.Either.Unwrap
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Control.Monad
import Control.Monad.Instances
import Control.Monad.Identity
import Control.Applicative
import TinyRoguelike.LevelParser
import Text.ParserCombinators.Parsec
import Text.Read

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

------------------------------------------------
-- Basic data structures for the map
------------------------------------------------

data Floor = Stone | Lava deriving (Eq, Ord, Enum, Read)
data Wall = Brick | Rubble deriving (Eq, Ord, Enum, Read)
data Item = Sword | Ectoplasm deriving (Eq, Ord, Enum, Read)
data NpcRace = Player | Rat | Goblin deriving (Eq, Ord, Enum, Read)

type NpcId = Int
newtype Npc = Npc (NpcRace, NpcId) deriving Eq

npcRace (Npc (race, id)) = race
npcId (Npc (race, id)) = id

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
mkTile = Tile

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
moveObject getter setter p1 p2 = do
    o1 <- getter p1
    o2 <- getter p2
    if isJust o1 && isNothing o2
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
findOnLevel fn = foldGridM foldFn Nothing
    where
        foldFn acc pos t = if fn t then Just pos else acc

findNpc :: (Npc -> Bool) -> LevelOp (Maybe Pos)
findNpc fn = findOnLevel (\t -> match . _npc $ t)
    where
        match Nothing = False
        match (Just npc) = fn npc

findPlayer :: LevelOp (Maybe Pos)
findPlayer = findNpc match
    where
        match (Npc (Player, _)) = True
        match _ = False

type Pos = (Int, Int)

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

-- GameOp and NpcOp expose a random generator
class Monad m => RandomProvider m where
    getRandom :: (StdGen -> (ret, StdGen)) -> m ret

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

-- Level parsing and building


loadLevel :: String -> Either String Level
loadLevel levelData =
    case parseLevelDescription levelData of
        Left err   -> Left err
        Right desc -> buildLevel desc


buildLevel :: LevelDescription -> Either String Level
buildLevel desc = case runGridOp emptyLevel buildOp of
    (Left err, level) -> Left err
    (Right _, level)  -> Right level
    where
        emptyLevel = mkGrid (dimension desc) (mkTile Nothing Nothing Nothing Nothing)
        toObject Nothing _ = Right Nothing
        toObject (Just str) ty = maybe (Left ("Unrecognized " ++ ty ++ " : " ++ str)) (Right . Just) (readMaybe str)
        toAvatar Nothing _ = Right Nothing
        toAvatar (Just str) i = maybe (Left ("Unrecognized avatar : " ++ str)) (Right . Just . Npc . (, i)) (readMaybe str)
        buildOp = do
            tiles <- forM (zip [0..] (tiles desc)) $ \(i, ch) -> do
                -- TODO: Add failure support to GridOp
                let (Just (floorStr, itemStr, wallStr, avatarStr)) = lookup ch (table desc)
                let tileE = do
                    floor  <- toObject floorStr "floor"
                    item   <- toObject itemStr "item"
                    wall   <- toObject wallStr "wall"
                    avatar <- toAvatar avatarStr i
                    return $ mkTile floor item wall avatar
                case tileE of
                    Left err -> return tileE
                    Right tile -> setiM i tile >> return tileE
            let errors = map fromLeft . filter isLeft $ tiles
            unless (null errors) $
                fail (intercalate " - " errors)




-- The Quasi Quoter

quoteLevelE str = case parseLevelDescription str of
    Left err -> fail err
    Right desc -> case buildLevel desc of
        Left err -> fail err
        Right _ -> dataToExpQ (const Nothing) desc

level :: QuasiQuoter
level = QuasiQuoter { quoteExp = quoteLevelE }






