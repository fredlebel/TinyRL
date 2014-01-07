{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -XRankNTypes #-}

module Data.Grid
( Grid
, mkGrid
, GridOp
, getM
, setM
, sizeM
, foldGridM
, runGridOp
, execGridOp
, evalGridOp
) where


import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic as G
import Control.Monad.Identity
import Control.Monad.ST
import Control.Applicative

{-
class Grid g where
    size :: g o -> (Int, Int)
    get :: (Int, Int) -> g o -> o
    set :: (Int, Int) -> o -> g o -> g o
    foldl :: (a -> o -> a) -> a -> g o -> a
-}

data Grid o = Grid {
        _size :: (Int, Int),
        _vector :: V.Vector o
    }

instance (Show o) => Show (Grid o) where
    show g = show (_size g) ++
             "\n" ++
             V.ifoldl foldFn "" (_vector g)
        where foldFn acc i o = acc ++ (if i `mod` (width g) == 0 then "\n" else "") ++ (show o)

mkGrid (w, h) o = Grid
    {
        _size = (w, h),
        _vector = V.replicate (w * h) o
    }

{-
type Lens x a = Functor f => (a -> f a) -> (x -> f x)
set :: Lens x a -> a -> x -> x
set ln a x = runIdentity $ ln (\_ -> Identity a) x
--set ln a x = runIdentity $ ln (const . Identity $ a) x

over :: Lens x a -> (a -> a) -> x -> x
over ln fn x = runIdentity $ ln (Identity . fn) x

view :: Lens x a -> x -> a
view ln x = getConst $ ln Const x
-}

--at :: (Int, Int) -> Lens (Grid o) o
--at pos fn g = (_set g pos) <$> (fn $ _get g pos)

width g = w where (w, h) = _size g
height g = h where (w, h) = _size g
toIndex g (x, y) = y * (width g) + x

--_get g pos    = (_vector g) V.! (toIndex g pos)
--_set g pos !o = g { _vector = (_vector g) V.// [(index, o)] }
--    where
--        index = toIndex g pos

over_all :: (o -> o) -> Grid o -> Grid o
over_all fn g = g { _vector = V.map fn (_vector g) }

data GridOp o r = GridOpCtor { gridOpFn :: forall s. MV.STVector s o -> Grid o -> ST s r }

instance Functor (GridOp o) where
    fmap f m = GridOpCtor $ \mv g -> f <$> gridOpFn m mv g

instance Monad (GridOp o) where
    return r = GridOpCtor $ \mv g -> return r
    m >>= fn = GridOpCtor $ \mv g -> do
        r <- gridOpFn m mv g
        gridOpFn (fn r) mv g


getM :: (Int, Int) -> GridOp o o
getM (x, y) = GridOpCtor $ \mv g -> MV.read mv (y * (width g) + x)


setM :: (Int, Int) -> o -> GridOp o ()
setM (x, y) o = GridOpCtor $ \mv g -> MV.write mv (y * (width g) + x) o

sizeM :: GridOp o (Int, Int)
sizeM = GridOpCtor $ \_ g -> return . _size $ g


foldGridM :: (a -> (Int, Int) -> o -> a) -> a -> GridOp o a
foldGridM fn a = GridOpCtor $ \mv g -> do
    v <- V.freeze mv
    return $ V.ifoldl (foldFn $ _size g) a v
    where
        foldFn (w, h) acc i o = fn acc (i `mod` w, i `div` h) o

runGridOp :: Grid o -> GridOp o r -> (r, Grid o)
runGridOp g op = runST $ do
    mv <- V.thaw (_vector g)
    r <- gridOpFn op mv g
    v <- V.freeze mv
    return (r, g { _vector = v })

evalGridOp g op = fst (runGridOp g op)
execGridOp g op = snd (runGridOp g op)


