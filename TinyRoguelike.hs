{-# OPTIONS_GHC -XRankNTypes #-}

import System.IO
import Data.Grid
import Data.Maybe
import qualified Data.Vector.Mutable as MV
import Control.Monad
import Control.Monad.Operation
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

data Floor = Stone | Lava deriving (Eq, Ord, Enum)
data Wall = Brick | Rubble deriving (Eq, Ord, Enum)
data Item = Sword | Ectoplasm deriving (Eq, Ord, Enum)
data NpcRace = Player | Rat | Goblin deriving (Eq, Ord, Enum)

type NpcId = Int
newtype Npc = Npc (NpcRace, NpcId)

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

type Level = Grid Tile
type LevelOp = GridOp Tile

fillGrid :: Tile -> LevelOp ()
fillGrid o = do
    (w, h) <- sizeM
    forM_ [0..h-1] $ \y -> do
        forM_ [0..w-1] $ \x -> do
            setM (x, y) o

getObject :: (Tile -> o) -> (Int, Int) -> LevelOp o
getObject fn pos = do
    t <- getM pos
    return $ fn t

getFloor = getObject _floor
getItem = getObject _item
getWall = getObject _wall
getNpc = getObject _npc

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

--moveObject :: ((Int, Int) -> WorldOp (Maybe o)) -> ((Int, Int) -> (Maybe o) -> WorldOp ()) -> (Int, Int) -> (Int, Int) -> WorldOp ()
moveObject getter setter p1 p2 = do
    o1 <- getter p1
    o2 <- getter p2
    when (isJust o1 && isNothing o2) $ do
        setter p1 Nothing
        setter p2 o1

moveNpc = moveObject getNpc setNpc

{-
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
-}

populate :: GameOp ()
populate = do
    runLevelOp $ do
        fillGrid $ mkTile (Just Stone) Nothing Nothing Nothing
        setNpc (2, 3) (Just (Npc (Player, 1)))
        setNpc (3, 4) (Just (Npc (Goblin, 100)))
        setWall (2, 4) (Just Brick)
        o <- getWall (2, 4)
        setWall (1, 2) o
    return ()

printWorld :: LevelOp String
printWorld = do
    (w, h) <- sizeM
    str <- foldGridM foldFn ('|' : replicate w '-')
    return $ str ++ ("|\n|" ++ replicate w '-' ++ "|")
    where
        foldFn acc (0, _) t = acc ++ "|\n|" ++ show t
        foldFn acc _      t = acc ++ show t

{-
movePlayer :: (Int, Int) -> Game -> Game
movePlayer pos g = g
    { _playerPos = pos
    , _world = execGridOp (_world g) (moveTopObject (_playerPos g) pos)
    }
-}

{-
type GameScript = WorldOp ()

class RoguelikeGame g where
    getPlayerPos :: g -> (Int, Int)
    movePlayer :: (Int, Int) -> g -> g
    execScript :: GameScript -> g -> g

playerPos :: Lens Game (Int, Int)
playerPos fn game = setPlayerPos <$> modPlayerPos
    where
        setPlayerPos (x, y) = game
            { _playerPos = (x, y)
            , _world =
            }
-}

findPlayer :: LevelOp (Maybe Pos)
findPlayer = do
    pos <- foldGridM foldFn Nothing
    return pos
    where
        foldFn acc pos (Tile _ _ _ (Just (Npc (Player, _)))) = Just pos
        foldFn acc _ _ = acc

movePlayer ch = do
    playerPos <- findPlayer
    when (isJust playerPos) (doMove ch playerPos)
    setWall (0, 0) (Just Brick)
    where
        doMove 'w' (Just (x, y)) = moveNpc (x, y) (x, y-1)
        doMove 'a' (Just (x, y)) = moveNpc (x, y) (x-1, y)
        doMove 's' (Just (x, y)) = moveNpc (x, y) (x, y+1)
        doMove 'd' (Just (x, y)) = moveNpc (x, y) (x+1, y)
        doMove _ _ = return ()

{-gameLoop grid = do
    ch <- getChar
    let newGrid = execGridOp grid (movePlayer ch)
    putStrLn $ evalGridOp newGrid printWorld
    (print . evalGridOp newGrid) findPlayer
    gameLoop newGrid-}

main :: IO ()
main = do
    {-hSetBuffering stdin NoBuffering
    --let g = execGridOp (mkGrid (30, 10) []) populate
    --(putStrLn . evalGridOp g) printWorld
    --putStrLn ""
    let step0 = mkGrid (30, 10) (mkTile Nothing Nothing Nothing Nothing)

    let step1 = execGridOp step0 populate
    (putStrLn . evalGridOp step1) printWorld
    (print . evalGridOp step1) findPlayer

    let step2 = execGridOp step1 (moveNpc (2,3) (3,3))
    (putStrLn . evalGridOp step2) printWorld
    (print . evalGridOp step2) findPlayer

    let step3 = execGridOp step2 (moveNpc (3,3) (3,4))
    (putStrLn . evalGridOp step3) printWorld
    (print . evalGridOp step3) findPlayer-}

    --gameLoop step3
    --(putStrLn . evalGridOp (_worldMap game)) (populate >> moveNpc (2,3) (3,3) >> printWorld)
    --(putStrLn . evalGridOp (movePlayer (3, 3) g)) printWorld
    
    let floorTile = mkTile (Just Stone) Nothing Nothing Nothing
    let game = GameStateCtor (mkGrid (30, 10) floorTile) [] []
    
    gameLoop $ execGameOp game populate

    return ()

--------------------------------------------------

{-
class Operation o where
    opFn :: o d r -> r

data Op (o d r) = OpCtor { _opFn :: (Operation o) => o d r -> r }

instance Operation (Op o d) where
    opFn = _opFn

instance (Operation o) => Monad (Op o d) where
    return r = OpCtor $ \d -> return r
    m >>= fn = OpCtor $ \d ->
        let r = opFn m
        in (fn r)

data Game
data GameOpT
type GameOp = Op GameOpT Game
-}

{-
mkGame :: (Int, Int) -> Game
mkGame dim = Game
    { _playerPos = Nothing
    , _worldMap = mkGrid dim (mkTile Nothing Nothing Nothing Nothing)
    }
-}

data GameState = GameStateCtor
    { _worldMap :: Grid Tile
    , _inventory :: [Item]
    , _messages :: [String]
    }

type Pos = (Int, Int)
{-
data GameOpT
type GameOp = OperationST GameOpT (GameState, forall s. MV.MVector s Tile)
-}

data GameOp r = GameOpCtor { _gameOpFn :: GameState -> LevelOp (r, GameState) }

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
        
runLevelOp :: LevelOp r -> GameOp r
runLevelOp op = GameOpCtor $ \game -> do
    ret <- op
    return (ret, game)
    
    


foldLevelM :: (acc -> Pos -> Tile -> acc) -> acc -> GameOp acc
foldLevelM fn acc = GameOpCtor $ \game -> do
    ret <- foldGridM fn acc
    return (ret, game)

--    (w, h) <- sizeM tiles
--    forM [0..h-1] $ \y -> do
--        forM [0..w-1] $ \x -> do

foldNpcs :: (acc -> Pos -> Npc -> acc) -> acc -> GameOp acc
foldNpcs fn acc = do
    foldLevelM foldFn acc
    where
        foldFn acc pos (Tile _ _ _ (Just npc)) = fn acc pos npc
        foldFn acc _ _ = acc

--instance MakeableOp GameOpT

foo :: Int -> GameOp Bool
foo n = do
    return (n > 2)

bar :: Int -> GameOp Bool
bar n = GameOpCtor $ \game -> return (n > 2, game)


--data NpcOpT
--type NpcOpM = OperationST NpcOpT (GameState, forall s. MV.MVector s Tile, Pos)

--runNpcOp :: GameState -> Pos -> NpcOpM r -> GameOp r
--runNpcOp game pos op = OpCtor $ \(game, mv) ->
--    runOperationST op (game, mv, pos)



--walk :: Direction -> NpcOpT ()
--walk dir = OpCtor $ \(game, mv, pos) -> moveNpc

--showMessage :: String -> GameOp ()
--runGridOpM :: GridOp r -> GameOp r

data Direction = Up | Down | Left | Right

playerAct :: Char -> GameOp ()
playerAct ch = do
    Just pos <- runLevelOp findPlayer
    runLevelOp $ movePlayer ch pos
    where
    movePlayer 'w' (x, y) = moveNpc (x, y) (x, y-1)
    movePlayer 'a' (x, y) = moveNpc (x, y) (x-1, y)
    movePlayer 's' (x, y) = moveNpc (x, y) (x, y+1)
    movePlayer 'd' (x, y) = moveNpc (x, y) (x+1, y)
    movePlayer _ _ = return ()

findAllNpcs :: GameOp [Npc]
findAllNpcs = foldNpcs foldFn []
    where
        foldFn acc pos npc = npc : acc

--move :: Direction -> NpcOp ()
gameLoop :: GameState -> IO GameState
gameLoop game = do
    ch <- getChar
    let (render, game') = runGameOp game $ do
        (playerAct ch)
        npcs <- findAllNpcs
        --forM_ npcs $ \npc -> do
        --    runNpcOp npcAct npc
        render <- runLevelOp printWorld
        return render
    putStrLn render
    gameLoop game'
{-
-}




























