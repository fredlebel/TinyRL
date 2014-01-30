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
import UI.HSCurses.Curses
import UI.HSCurses.CursesHelper


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

printLevel :: GameOp [String]
printLevel = runLevelOp $ do
    (w, h) <- sizeM
    let topBottomRow = "|" ++ (replicate w '-') ++ "|"
    rows <- foldGridM foldFn []
    return $ [topBottomRow] ++ rows ++ [topBottomRow]
    where
        foldFn :: [String] -> Pos -> Tile -> [String]
        foldFn [] (0, _) t = ["|" ++ show t]
        foldFn acc (0, _) t = (init acc) ++ [last acc ++ "|"] ++ ["|" ++ show t]
        foldFn acc _      t = (init acc) ++ [last acc ++ show t]

findPlayer :: LevelOp (Maybe Pos)
findPlayer = do
    pos <- foldGridM foldFn Nothing
    return pos
    where
        foldFn acc pos (Tile _ _ _ (Just (Npc (Player, _)))) = Just pos
        foldFn acc _ _ = acc


main :: IO ()
main = do
    -- Curses setup
    initCurses
    cBreak True
    flushinp
    --cursSet CursorInvisible
    echo False
    win <- newWin 25 80 0 0
    keypad win True
    wclear win
    wRefresh win
    refresh

    let floorTile = mkTile (Just Stone) Nothing Nothing Nothing
    let game = GameStateCtor (mkGrid (78, 16) floorTile) [] [] (mkStdGen 1234)

    gameLoop win (execGameOp game populate) 0

    wclear win
    wMove win 0 0
    wAddStr win "    Game ended    "
    wRefresh win
    getch
    delWin win
    endWin
    update
    return ()

--------------------------------------------------



foldNpcs :: (acc -> Pos -> Npc -> acc) -> acc -> GameOp acc
foldNpcs fn acc = do
    foldLevelM foldFn acc
    where
        foldFn acc pos (Tile _ _ _ (Just npc)) = fn acc pos npc
        foldFn acc _ _ = acc

--instance MakeableOp GameOpT

playerAct :: Key -> GameOp Bool
playerAct ch = do
    Just (x, y) <- runLevelOp findPlayer
    let newPos = case ch of
                    KeyChar 'w' -> (x, y-1)
                    KeyChar 'a' -> (x-1, y)
                    KeyChar 's' -> (x, y+1)
                    KeyChar 'd' -> (x+1, y)
                    _   -> (x, y)
    when ((x, y) /= newPos) $ do
        runLevelOp $ moveNpc (x, y) newPos
        logMessage "Player moved"
    if ch == KeyChar 'q'
        then return False
        else return True


findAllNpcs :: GameOp [Npc]
findAllNpcs = foldNpcs foldFn []
    where
        foldFn acc pos npc = npc : acc

gameLoop :: Window -> GameState -> Int -> IO GameState
gameLoop win game frameNum = do

    -- Print the level
    wclear win
    let ((render, messages), _) = runGameOp game $ do
        r <- printLevel
        m <- getLastFrameMessages
        return (r, m)
    forM_ (zip [0..] render) $ \(i, line) -> do
        wMove win i 0
        wAddStr win line
    forM_ (zip [0..] messages) $ \(i, line) -> do
        wMove win ((length render) + i) 0
        wAddStr win line
    wMove win 0 0
    wAddStr win (show frameNum)
    wRefresh win
    --refresh
    --update

    print "Before"
    ch <- getCh
    print ch

    let (mustQuit, game') = runGameOp game $ do
        beginMessageFrame
        mustQuit <- not <$> (playerAct ch)
        npcs <- findAllNpcs
        forM_ npcs $ \npc -> do
            runNpcOp npc npcAct
        return mustQuit

    if mustQuit
        then return game'
        else gameLoop win game' (frameNum + 1)

npcAct :: NpcOp ()
npcAct = do
    (Npc (race, id), pos) <- whoAmI
    if race /= Player
    then do
        rndDir <- getRandom random
        npcWalk rndDir
        return ()
    else return ()







{-

    00000000001111111111222222222233333333334444444444555555555566666666667777777777
    01234567890123456789012345678901234567890123456789012345678901234567890123456789

 00 ################################################################################
 01 ################################################################################
 02 ################################################################################
 03 ################################################################################
 04 ################################################################################
 05 ################################################################################
 06 ################################################################################
 07 ################################################################################
 08 ################################################################################
 09 ################################################################################
 10 ################################################################################
 11 ################################################################################
 12 ################################################################################
 13 ################################################################################
 14 ################################################################################
 15 ################################################################################
 16 ################################################################################
 17 ################################################################################
 18 ################################################################################
 19 ################################################################################

    00000000001111111111222222222233333333334444444444555555555566666666667777777777
    01234567890123456789012345678901234567890123456789012345678901234567890123456789

 00 ################################################################################
 01 #+------+------------------------+##############################################
 02 #|Square|                        |##############################################
 03 #| 8x5  O                        +------------------------+#####################
 04 #|      |       OddShape_1       O                        |#####################
 05 #++-O-+-+                        |   Rectangle_25x7       +------+##############
 06 ##|   |#+------+         +-------+                        O daeD |##############
 07 ##|   |########|         |+------+                        |  dnE |##############
 08 ##|   |########|         || Dead O                        |    1 |##############
 09 ##+-O-+----+---+----O-+--+| End  +------------------------+------+##############
 10 ##|        |          |###| 1    |##############################################
 11 ##|        O          |###+------+##############################################
 12 ##|        |          |#########################################################
 13 ##+--------+          |#########################################################
 14 ###########+----------+#########################################################
 15 ################################################################################
 16 ################################################################################
 17 ################################################################################
 18 ################################################################################
 19 ################################################################################


 DefineRoomShape OddShape_1
 +--------------------------+
 |##########################|
 |#........................#|
 |O........................#|
 |#........................O|
 |#........................#|
 |########.........#########|
 |       #.........#        |
 |       #.........#        |
 |       #####O#####        |
 +--------------------------+

 DefineLevelLayout Misc_1

    (1,1) -> NoFlip Square_8x5
    (8,1) -> NoFlip OddShape_1
    ...
    7 -> NoFlip DeadEnd1
    8 -> FlipH DeadEnd1

    ################################################################################
    #1------2------------------------+##############################################
    #|      |                        |##############################################
    #|      O                        3------------------------+#####################
    #|      |                        O                        |#####################
    #+4-O-+-+                        |                        8------+##############
    ##|   |#+------+         +-------+                        O      |##############
    ##|   |########|         |7------+                        |      |##############
    ##|   |########|         ||      O                        |      |##############
    ##5-O-+----6---+----O-+--+|      +------------------------+------+##############
    ##|        |          |###|      |##############################################
    ##|        O          |###+------+##############################################
    ##|        |          |#########################################################
    ##+--------+          |#########################################################
    ###########+----------+#########################################################
    ################################################################################
    ################################################################################
    ################################################################################
    ################################################################################
    ################################################################################


 Rectangle_25x7
 #########################
 O       #######        ##
 ##      #####    ###    #
 ##               ###    O
 ##      #####    ###    #
 O       #######        ##
 #########################

 ########
 ##     O
 ##     #
 ##^#^#^#
 ########



-}


















