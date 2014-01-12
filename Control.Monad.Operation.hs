{-# OPTIONS_GHC -XRankNTypes #-}

module Control.Monad.Operation
( Operation(OpCtor)
, OperationST(OpCtorST)
, runOperationST
) where

import Control.Monad.ST
import Data.STRef

-- Typed state monad
data Operation t state ret = OpCtor { _opFn :: state -> (ret, state) }

instance Monad (Operation t state) where
    return ret = OpCtor $ \state -> (ret, state)
    m >>= fn = OpCtor $ \state ->
        let (ret, state') = _opFn m state
        in _opFn (fn ret) state'

-- Typed state monad contained in ST monad.
data OperationST t state ret = OpCtorST { _opFnST :: forall s. STRef s state -> ST s ret }

instance Monad (OperationST t state) where
    return ret = OpCtorST $ \state -> return ret
    m >>= fn = OpCtorST $ \state -> do
        ret <- _opFnST m state
        _opFnST (fn ret) state

runOperationST :: OperationST t state ret -> state -> (ret, state)
runOperationST op state = (ret, state')
    where
        (ret, state') = runST $ do
            stateVar <- newSTRef state
            ret <- (_opFnST op) stateVar
            state' <- readSTRef stateVar
            return (ret, state')

--joinOperationST :: OperationST t1 state1 ret -> state1 -> OperationST t2 state2 (ret, state1)
--joinOperationST op state = _opFnST op 