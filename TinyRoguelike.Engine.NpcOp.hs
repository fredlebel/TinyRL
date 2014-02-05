{-# OPTIONS_GHC -XRankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module TinyRoguelike.Engine.NpcOp
( NpcOp, runNpcOp
, runLevelOp
, npcWalk, whoAmI
) where

import TinyRoguelike.Engine
import TinyRoguelike.Engine.GameOp
import Control.Applicative
import Data.Grid
import Data.Maybe


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
    Just pos <- runLevelOp $ findNpc (==npc)
    (r, _) <- _npcOpFn op pos
    return r

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
            logMessage (str)
            return False
        _ -> do
            _ <- runLevelOp $ moveNpc oldPos newPos
            return True
    return (ret, if ret then newPos else oldPos)

whoAmI :: NpcOp (Npc, Pos)
whoAmI = NpcOpCtor $ \pos -> do
    Just npc <- runLevelOp $ getNpc pos
    return ((npc, pos), pos)

instance RandomProvider NpcOp where
    getRandom fn = NpcOpCtor $ \pos -> do
        ret <- getRandom fn
        return (ret, pos)

instance MessageLogger NpcOp where
    beginMessageFrame     = NpcOpCtor $ \pos -> (, pos) <$> beginMessageFrame
    logMessage msg        = NpcOpCtor $ \pos -> (, pos) <$> logMessage msg
    getLastFrameMessages  = NpcOpCtor $ \pos -> (, pos) <$> getLastFrameMessages
    getAllMessages        = NpcOpCtor $ \pos -> (, pos) <$> getAllMessages
    clearOldMessageFrames = NpcOpCtor $ \pos -> (, pos) <$> clearOldMessageFrames






