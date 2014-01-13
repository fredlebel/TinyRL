{-# OPTIONS_GHC -XRankNTypes #-}

import System.IO
import Data.Grid
import Data.Maybe
import qualified Data.Vector.Mutable as MV
import Control.Monad
import Control.Monad.Operation
import Control.Monad.Identity
import Control.Applicative
import TinyRoguelike.Engine


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

populate :: GameOp ()
populate = do
    runLevelOp $ do
        fillM $ mkTile (Just Stone) Nothing Nothing Nothing
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

findPlayer :: LevelOp (Maybe Pos)
findPlayer = do
    pos <- foldGridM foldFn Nothing
    return pos
    where
        foldFn acc pos (Tile _ _ _ (Just (Npc (Player, _)))) = Just pos
        foldFn acc _ _ = acc

movePlayer ch = do
    playerPos <- findPlayer
    when (isJust playerPos) (void $ doMove ch playerPos)
    where
        doMove 'w' (Just (x, y)) = moveNpc (x, y) (x, y-1)
        doMove 'a' (Just (x, y)) = moveNpc (x, y) (x-1, y)
        doMove 's' (Just (x, y)) = moveNpc (x, y) (x, y+1)
        doMove 'd' (Just (x, y)) = moveNpc (x, y) (x+1, y)
        doMove _ _ = return False

main :: IO ()
main = do

    let floorTile = mkTile (Just Stone) Nothing Nothing Nothing
    let game = GameStateCtor (mkGrid (30, 10) floorTile) [] []
    
    gameLoop $ execGameOp game populate

    return ()

--------------------------------------------------



foldNpcs :: (acc -> Pos -> Npc -> acc) -> acc -> GameOp acc
foldNpcs fn acc = do
    foldLevelM foldFn acc
    where
        foldFn acc pos (Tile _ _ _ (Just npc)) = fn acc pos npc
        foldFn acc _ _ = acc

--instance MakeableOp GameOpT

playerAct :: Char -> GameOp ()
playerAct ch = do
    Just pos <- runLevelOp findPlayer
    runLevelOp $ movePlayer ch pos
    return ()
    where
    movePlayer 'w' (x, y) = moveNpc (x, y) (x, y-1)
    movePlayer 'a' (x, y) = moveNpc (x, y) (x-1, y)
    movePlayer 's' (x, y) = moveNpc (x, y) (x, y+1)
    movePlayer 'd' (x, y) = moveNpc (x, y) (x+1, y)
    movePlayer _ _ = return False

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
        forM_ npcs $ \npc -> do
            runNpcOp npc npcAct
        render <- runLevelOp printWorld
        return render
    putStrLn render
    gameLoop game'
{-
-}

npcAct :: NpcOp ()
npcAct = do
    return ()


























