{-# OPTIONS_GHC -XRankNTypes #-}

import Data.Grid
import Control.Monad
import Control.Monad.Identity
import Control.Applicative


------------------------------------------
-- Basic lens
type Lens x a = Functor f => (a -> f a) -> (x -> f x)
set :: Lens x a -> a -> x -> x
set ln a x = runIdentity $ ln (const . Identity $ a) x

over :: Lens x a -> (a -> a) -> x -> x
over ln fn x = runIdentity $ ln (Identity . fn) x

view :: Lens x a -> x -> a
view ln x = getConst $ ln Const x
------------------------------------------

data Floor = Stone | Lava
data Wall = Brick | Rubble
data Item = Dagger | Sword | Gold
data NPC = Rat | Goblin

data WorldTile2 = WorldTile2
    { _floor :: Maybe Floor
    , _item :: Maybe Item
    , _wall :: Maybe Wall
    , _npc :: Maybe NPC
    }

data Game = Game
    { _playerPos :: (Int, Int)
    , _worldMap :: Grid WorldTile2
    }

data Object = Player | NPC | Wall | Weapon | Floor
    deriving (Eq, Read, Enum)

instance Show Object where
    show Player = "@"
    show NPC    = "M"
    show Wall   = "#"
    show Weapon = "/"
    show Floor  = "."

type WorldTile = [Object]
type WorldMap = Grid WorldTile
type WorldOp = GridOp WorldTile

fillGrid :: WorldTile -> WorldOp ()
fillGrid o = do
    (w, h) <- sizeM
    forM_ [0..h-1] $ \y -> do
        forM_ [0..w-1] $ \x -> do
            setM (x, y) o

pushObject :: (Int, Int) -> Object -> WorldOp ()
pushObject pos o = do
    os <- getM pos
    setM pos (o : os)

popObject :: (Int, Int) -> WorldOp Object
popObject pos = do
    os <- getM pos
    setM pos (tail os)
    return (head os)
    
moveTopObject :: (Int, Int) -> (Int, Int) -> WorldOp ()
moveTopObject p1 p2 = do
    o <- popObject p1
    pushObject p2 o
            
populate :: WorldOp ()
populate = do
    fillGrid [Floor]
    pushObject (2, 3) Player
    pushObject (2, 4) Wall
    o <- getM (2, 4)
    setM (1, 2) o

printWorld :: WorldOp String
printWorld = do
    (w, h) <- sizeM
    str <- foldGridM foldFn ('|' : replicate w '-')
    return $ str ++ ("|\n|" ++ replicate w '-' ++ "|")
    where
        foldFn acc (0, _) [] = acc ++ "|\n|" ++ " "
        foldFn acc (0, _) o = acc ++ "|\n|" ++ (show . head) o
        foldFn acc _      [] = acc ++ " "
        foldFn acc _      o = acc ++ (show . head) o

{-
movePlayer :: (Int, Int) -> Game -> Game
movePlayer pos g = g
    { _playerPos = pos
    , _world = execGridOp (_world g) (moveTopObject (_playerPos g) pos)
    }
-}

type GameScript = WorldOp ()

class RoguelikeGame g where
    getPlayerPos :: g -> (Int, Int)
    movePlayer :: (Int, Int) -> g -> g
    execScript :: GameScript -> g -> g

{-    
playerPos :: Lens Game (Int, Int)
playerPos fn game = setPlayerPos <$> modPlayerPos
    where
        setPlayerPos (x, y) = game
            { _playerPos = (x, y)
            , _world = 
            }
-}

main :: IO ()
main = do
    let g = execGridOp (mkGrid (30, 10) []) populate
    (putStrLn . evalGridOp g) printWorld
    putStrLn ""
    (putStrLn . evalGridOp g) (moveTopObject (2,3) (3,3) >> printWorld)
    --(putStrLn . evalGridOp (movePlayer (3, 3) g)) printWorld

    return ()
