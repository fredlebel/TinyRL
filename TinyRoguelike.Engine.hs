{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module TinyRoguelike.Engine
( Floor (..)
, Wall (..), blocksMove
, Item (..)
, NpcRace (..)
, NpcId
, Npc (Npc), npcRace, npcId
, Tile (..)
, mkTile
, Level
, LevelOp
, Pos, offsetPos
, Direction (..)
, RandomProvider (..)
, MessageLogger (..)
, buildLevel
, level
) where

import Prelude hiding (floor, min, max)
import System.Random
import Data.Grid
import Data.List
import Data.Either.Unwrap
import Control.Monad
import TinyRoguelike.LevelParser
import Text.Read

import Language.Haskell.TH hiding (match)
import Language.Haskell.TH.Quote

------------------------------------------------
-- Basic data structures for the map
------------------------------------------------

data Floor = Stone | Lava deriving (Eq, Ord, Enum, Read)
data Wall = Brick | Rubble | ClosedDoor | OpenedDoor deriving (Eq, Ord, Enum, Read)
data Item = Sword | Gun deriving (Eq, Ord, Enum, Read)
data NpcRace = Player | Rat | Goblin deriving (Eq, Ord, Enum, Read)

blocksMove :: Wall -> Bool
blocksMove Brick      = True
blocksMove Rubble     = True
blocksMove ClosedDoor = True
blocksMove OpenedDoor = False

type NpcId = Int
newtype Npc = Npc (NpcRace, NpcId) deriving Eq

npcRace (Npc (race, _)) = race
npcId (Npc (_, theId)) = theId

-- Show instances for debugging purposes only
instance Show Floor where
    show Stone = "."
    show Lava = "~"

instance Show Wall where
    show Brick      = "#"
    show Rubble     = "%"
    show ClosedDoor = "+"
    show OpenedDoor = "/"

instance Show Item where
    show Sword = "\\"
    show Gun = "}"

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


type Pos = (Int, Int)

data Direction = North | East | South | West
    deriving (Eq, Bounded, Enum)

offsetPos :: Direction -> Pos -> Pos
offsetPos dir (x, y) = case dir of
                            North -> (x, y-1)
                            South -> (x, y+1)
                            East  -> (x+1, y)
                            West  -> (x-1, y)

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

{-
loadLevel :: String -> Either String Level
loadLevel levelData =
    case parseLevelDescription levelData of
        Left err   -> Left err
        Right desc -> buildLevel desc
-}

buildLevel :: LevelDescription -> Either String Level
buildLevel desc = case runGridOp emptyLevel buildOp of
    (Left err, _)   -> Left err
    (Right _, lvl)  -> Right lvl
    where
        emptyLevel = mkGrid (dimension desc) (mkTile Nothing Nothing Nothing Nothing)
        toObject Nothing _ = Right Nothing
        toObject (Just str) ty = maybe (Left ("Unrecognized " ++ ty ++ " : " ++ str)) (Right . Just) (readMaybe str)
        toAvatar Nothing _ = Right Nothing
        toAvatar (Just str) i = maybe (Left ("Unrecognized avatar : " ++ str)) (Right . Just . Npc . (, i)) (readMaybe str)
        lookupTileDef ch tbl = case lookup ch tbl of
                                    Nothing -> Left ("Unknown tile: " ++ [ch])
                                    Just t  -> Right t
        buildOp = do
            allTiles <- forM (zip [0..] (tiles desc)) $ \(i, ch) -> do
                -- TODO: Add failure support to GridOp
                --let (Just (floorStr, itemStr, wallStr, avatarStr)) = lookup ch (table desc)
                let tileE = do
                    (floorStr, itemStr, wallStr, avatarStr) <- lookupTileDef ch (table desc)
                    floor  <- toObject floorStr "floor"
                    item   <- toObject itemStr "item"
                    wall   <- toObject wallStr "wall"
                    avatar <- toAvatar avatarStr i
                    return $ mkTile floor item wall avatar
                case tileE of
                    Left _ -> return tileE
                    Right tile -> setiM i tile >> return tileE
            let errors = map fromLeft . filter isLeft $ allTiles
            unless (null errors) $
                fail (intercalate " - " errors)




-- The Quasi Quoter
quoteLevelE :: String -> Q Exp
quoteLevelE str = case parseLevelDescription str of
    Left err -> fail err
    Right desc -> case buildLevel desc of
        Left err -> fail err
        Right _ -> dataToExpQ (const Nothing) desc

level :: QuasiQuoter
level = QuasiQuoter
    { quoteExp = quoteLevelE
    , quotePat = fail
    , quoteType = fail
    , quoteDec = fail
    }





