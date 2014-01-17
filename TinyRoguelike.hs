{-# OPTIONS_GHC -XRankNTypes #-}
{-# LANGUAGE MultiWayIf #-}

import System.IO
import System.Random
import Data.Grid
import Data.Maybe
import Data.List
import qualified Data.Vector.Mutable as MV
import Control.Monad
--import Control.Monad.Operation
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
        setNpc (3, 5) (Just (Npc (Goblin, 101)))
        setNpc (3, 6) (Just (Npc (Goblin, 102)))
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


main :: IO ()
main = do

    let floorTile = mkTile (Just Stone) Nothing Nothing Nothing
    let game = GameStateCtor (mkGrid (30, 10) floorTile) [] [] (mkStdGen 1234)
    
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
    Just (x, y) <- runLevelOp findPlayer
    let newPos = case ch of
                    'w' -> (x, y-1)
                    'a' -> (x-1, y)
                    's' -> (x, y+1)
                    'd' -> (x+1, y)
                    _   -> (x, y)
    when ((x, y) /= newPos) $ do
        runLevelOp $ moveNpc (x, y) newPos
        logMessage "Player moved"
    return ()
    

findAllNpcs :: GameOp [Npc]
findAllNpcs = foldNpcs foldFn []
    where
        foldFn acc pos npc = npc : acc

gameLoop :: GameState -> IO GameState
gameLoop game = do
    ch <- getChar
    
    let (render, game') = runGameOp game $ do
        beginMessageFrame
        (playerAct ch)
        npcs <- findAllNpcs
        --foldM (\gen npc -> runNpcOp npc (npcAct gen)) rnd npcs
        forM_ npcs $ \npc -> do
            runNpcOp npc npcAct
        render <- runLevelOp printWorld
        messages <- (concat . intersperse "\n") <$> getLastFrameMessages
        return (messages ++ "\n" ++ render)
    putStrLn render
    gameLoop game'

npcAct :: NpcOp ()
npcAct = do
    (Npc (race, id), pos) <- whoAmI
    if race /= Player
    then do
        rndDir <- getRandom random
        npcWalk rndDir
        return ()
    else return ()


























