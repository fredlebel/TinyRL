{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module TinyRoguelike.Engine.GameOp
( GameOp, runGameOp, execGameOp, evalGameOp
, GameState
, mkGame
, runLevelOp
, foldLevelM, foldNpcs
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
import TinyRoguelike.Engine
import Text.ParserCombinators.Parsec
import Text.Read

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse


data GameState = GameStateCtor
    { _worldMap :: Grid Tile
    , _inventory :: [Item]
    , _messages :: [[String]]
    , _rnd :: StdGen
    }

mkGame = GameStateCtor

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
        -- TODO: Error propagation.
        (Right (ret, game'), newGrid) = runGridOp (_worldMap game) (_gameOpFn op game)
        game'' = game' { _worldMap = newGrid }

evalGameOp game op = fst $ runGameOp game op
execGameOp game op = snd $ runGameOp game op

foldLevelM :: (acc -> Pos -> Tile -> acc) -> acc -> GameOp acc
foldLevelM fn acc = GameOpCtor $ \game -> do
    ret <- foldGridM fn acc
    return (ret, game)

foldNpcs :: (acc -> Pos -> Npc -> acc) -> acc -> GameOp acc
foldNpcs fn = foldLevelM foldFn
    where
        foldFn acc pos (Tile _ _ _ (Just npc)) = fn acc pos npc
        foldFn acc _ _ = acc

runLevelOp :: LevelOp r -> GameOp r
runLevelOp op = GameOpCtor $ \game -> do
    ret <- op
    return (ret, game)

instance RandomProvider GameOp where
    getRandom fn = GameOpCtor $ \game -> do
        let (ret, g) = fn (_rnd game)
        return (ret, game { _rnd = g })

instance MessageLogger GameOp where
    beginMessageFrame = GameOpCtor $ \game -> do
        let msgs = _messages game
        return ((), game { _messages = [] : msgs })
    logMessage msg = GameOpCtor $ \game -> do
        let (frame:frames) = _messages game
        return ((), game { _messages = (msg:frame):frames })
    getLastFrameMessages = GameOpCtor $ \game -> do
        let (frame:frames) = _messages game
        return $ if null (_messages game)
            then ([], game)
            else (head . _messages $ game, game)
    getAllMessages = GameOpCtor $ \game ->
        return (_messages game, game)
    clearOldMessageFrames = GameOpCtor $ \game -> do
        let (frame:frames) = _messages game
        return ((), game { _messages = [frame] })






