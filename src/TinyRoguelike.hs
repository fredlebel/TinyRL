{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

import System.Random
import Data.Grid
import Control.Monad
import Engine.Engine
import Engine.GameOp
import Engine.NpcOp
import UI.HSCurses.Curses
--import FovPrecisePermissive


------------------------------------------
-- Basic lens
{-
type Lens x a = Functor f => (a -> f a) -> (x -> f x)
set :: Lens x a -> a -> x -> x
set ln a x = runIdentity $ ln (const . Identity $ a) x

over :: Lens x a -> (a -> a) -> x -> x
over ln fn x = runIdentity $ ln (Identity . fn) x

view :: Lens x a -> x -> a
view ln x = getConst $ ln Const x
-}
------------------------------------------

{-
printLevel :: GameOp [String]
printLevel = runLevelOp $ foldGridM foldFn []
    where
        foldFn :: [String] -> Pos -> Tile -> [String]
        foldFn []  (0, _) t = [show t]
        foldFn acc (0, _) t = acc ++ [show t]
        foldFn acc _      t = init acc ++ [last acc ++ show t]
-}

printLevel :: GameOp [String]
printLevel = do
    visibleTiles <- viewedTiles
    doFold visibleTiles
    --return [show visibleTiles]
    where
        doFold visibleTiles = runLevelOp $ foldGridM foldFn []
            where
                foldFn :: [String] -> Pos -> Tile -> [String]
                foldFn []  p@(0, _) t = if elem p visibleTiles then [show t] else [" "]
                foldFn acc p@(0, _) t = acc ++ if elem p visibleTiles then [show t] else [" "]
                foldFn acc p      t = init acc ++ [last acc ++ (if elem p visibleTiles then show t else " ")]
        check pos = do
            outOfBounds <- not <$> onLevel pos
            if outOfBounds
                then return True
                else maybe False blocksMove <$> getWall pos


main :: IO ()
main = do
    -- Curses setup
    initCurses
    cBreak True
    _ <- flushinp
    --cursSet CursorInvisible
    echo False
    win <- newWin 25 80 0 0
    keypad win True
    wclear win
    wRefresh win
    refresh

    let (Right firstLevel) = buildLevel [level|
        # = (Stone, _, Brick, _)
        . = (Stone, _, _, _)
        } = (Stone, Gun, _, _)
        + = (Stone, _, ClosedDoor, _)
        / = (Stone, _, OpenedDoor, _)
        @ = (Stone, _, _, Player)
        g = (Stone, _, _, Goblin)
        ~ = (Lava, _, _, _)
        (80x20)
        ################################################################################
        ##############################################....##...#...#...#...#############
        #########################............#########....##.g.#.g.#.g.#.g.#############
        #######################................###.........#...#...#...#...#############
        #######################..............}.###.........##+###/###/###+##############
        #######################....###..###.....+..........+...............#############
        #########...g.##.....##....###..###....###.........#...............#############
        #########.....##.g...##................###.........#############################
        #########.....##.....#####..........##########+#################################
        #########..g..##...g.#####..........####................########################
        #########.....##.....#####..........####................########################
        ############+#######+##########+########....~~~~~~~.....########################
        ########.........###.....ggg##...#######....~~~~~~~.....########################
        ######.............#........##...#######....~~~@~~~.....########################
        ######.........g...+........##...#######....~~~~~~~.....########################
        ######.............####+######...#######................########################
        ########.........######..........#######................########################
        #######################..........#######................########################
        ################################################################################
        ################################################################################|]

    let game = mkGame firstLevel [] [] [] (mkStdGen 1234)
    _ <- gameLoop win game 0
    wclear win
    wMove win 0 0
    wAddStr win "    Game ended    "
    wRefresh win
    _ <- getch
    delWin win
    endWin
    return ()

--------------------------------------------------


--instance MakeableOp GameOpT

data PlayerAction =
    Move Direction |
    OpenDoor Direction |
    CloseDoor Direction |
    QuitGame

data KeyCommand =
    CompleteCommand PlayerAction |
    WaitForDirection (Direction -> PlayerAction) |
    AbortedCommand

mapKeyCommand :: Key -> KeyCommand
mapKeyCommand = \case
    (KeyChar 'w') -> CompleteCommand $ Move North
    KeyUp         -> CompleteCommand $ Move North
    (KeyChar 'a') -> CompleteCommand $ Move West
    KeyLeft       -> CompleteCommand $ Move West
    (KeyChar 's') -> CompleteCommand $ Move South
    KeyDown       -> CompleteCommand $ Move South
    (KeyChar 'd') -> CompleteCommand $ Move East
    KeyRight      -> CompleteCommand $ Move East
    (KeyChar 'Q') -> CompleteCommand $ QuitGame
    (KeyChar 'c') -> WaitForDirection CloseDoor
    (KeyChar 'o') -> WaitForDirection OpenDoor
    _             -> AbortedCommand

promptForDirection :: IO (Maybe Direction)
promptForDirection = do
    ch <- getCh
    case ch of
        (KeyChar 'w') -> return $ Just North
        KeyUp         -> return $ Just North
        (KeyChar 'a') -> return $ Just West
        KeyLeft       -> return $ Just West
        (KeyChar 's') -> return $ Just South
        KeyDown       -> return $ Just South
        (KeyChar 'd') -> return $ Just East
        KeyRight      -> return $ Just East
        _             -> return $ Nothing

promptForPlayerAction :: IO PlayerAction
promptForPlayerAction = do
    ch <- getCh
    case mapKeyCommand ch of
        CompleteCommand cmd -> return cmd
        AbortedCommand      -> promptForPlayerAction
        WaitForDirection fn -> do
            chDir <- promptForDirection
            case chDir of
                Just dir -> return (fn dir)
                Nothing  -> promptForPlayerAction

checkActionContext :: PlayerAction -> GameOp PlayerAction
checkActionContext (Move direction) = do
    newPos <- offsetPos direction <$> findPlayer
    wall <- getWall newPos
    case wall of
        Just ClosedDoor -> return (OpenDoor direction)
        _               -> return (Move direction)
checkActionContext act = return act

-- Implementation of various player actions
playerAct :: PlayerAction -> GameOp ()
playerAct (Move direction) = do
    oldPos <- findPlayer
    let newPos = offsetPos direction oldPos
    canMove <- not <$> isTileBlocked newPos
    when canMove $ do
        _ <- moveNpc oldPos newPos
        maybeItem <- getItem newPos
        case maybeItem of
            Nothing -> return ()
            Just item -> addToInventory item
        setItem newPos Nothing
playerAct (OpenDoor direction) = do
    doorPos <- offsetPos direction <$> findPlayer
    -- TODO: GameOp failures
    wall <- getWall doorPos
    when (wall == Just ClosedDoor) $
        setWall doorPos (Just OpenedDoor)
playerAct (CloseDoor direction) = do
    doorPos <- offsetPos direction <$> findPlayer
    wall <- getWall doorPos
    when (wall == Just OpenedDoor) $
        setWall doorPos (Just ClosedDoor)

playerAct QuitGame = return ()


findAllNpcs :: GameOp [Npc]
findAllNpcs = foldNpcs foldFn []
    where
        foldFn acc _ npc = npc : acc


renderGame win game frameNum = do
    wclear win
    let frame = do
                    r <- printLevel
                    m <- getLastFrameMessages
                    return (r, m)
    -- Print the level
    let ((render, messages), _) = runGameOp game frame
    forM_ (zip [0..] render) $ \(i, line) -> do
        wMove win i 0
        wAddStr win line
    -- Print the items
    let items = evalGameOp game getInventory
    wMove win (length render) 0
    wAddStr win "Items: "
    forM_ items $ \item -> do
        wAddStr win (show item)
    -- Print the messages
    forM_ (zip [0..] messages) $ \(i, line) -> do
        wMove win (length render + i + 1) 0
        wAddStr win line
    wMove win 0 0
    wAddStr win (show frameNum)
    wRefresh win
    --refresh

gameLoop :: Window -> GameState -> Int -> IO GameState
gameLoop win game frameNum = do

    renderGame win game frameNum

    act <- promptForPlayerAction

    case act of
        QuitGame -> return game
        _        -> do
            let op = do
                        beginMessageFrame
                        checkActionContext act >>= playerAct
                        processFov
                        npcs <- findAllNpcs
                        forM_ npcs $ \npc -> runNpcOp npc npcAct
            let game' = execGameOp game op
            gameLoop win game' (frameNum + 1)

npcAct :: NpcOp ()
npcAct = do
    (Npc (race, _), _) <- whoAmI
    when (race /= Player) $ do
        rndDir <- getRandom random
        _ <- npcWalk rndDir
        return ()







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


















