{-# LANGUAGE RankNTypes #-}

module Data.Grid
( Grid
, mkGrid
, Pos
, GridOp
, width, height, widthM, heightM
, toIndex
, getM
, setM
, setiM
, sizeM
, containsM
, fillM
, foldGridM
, runGridOp
, execGridOp
, evalGridOp
) where


import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad.Identity
import Control.Monad.ST


data Grid o = GridCtor {
        _size :: (Int, Int),
        _vector :: V.Vector o
    }


data MGrid s o = MGridCtor {
        _sizeM :: (Int, Int),
        _vectorM :: MV.MVector s o
    }

instance (Show o) => Show (Grid o) where
    show g = show (_size g) ++
             "\n" ++
             V.ifoldl foldFn "" (_vector g)
        where foldFn acc i o = acc ++ (if i `mod` width g == 0 then "\n" else "") ++ show o

mkGrid :: forall o. (Int, Int) -> o -> Grid o
mkGrid (w, h) o = GridCtor
    {
        _size = (w, h),
        _vector = V.replicate (w * h) o
    }

type Pos = (Int, Int)

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

width :: forall o. Grid o -> Int
width   g = w where (w, _) = _size g

height :: forall o. Grid o -> Int
height  g = h where (_, h) = _size g

widthM :: forall s o. MGrid s o -> Int
widthM  g = w where (w, _) = _sizeM g

heightM :: forall s o. MGrid s o -> Int
heightM g = h where (_, h) = _sizeM g

toIndex :: forall o. Grid o -> Pos -> Int
toIndex g (x, y) = y * width g + x

--_get g pos    = (_vector g) V.! (toIndex g pos)
--_set g pos !o = g { _vector = (_vector g) V.// [(index, o)] }
--    where
--        index = toIndex g pos

data GridOp o r = GridOpCtor { gridOpFn :: forall s. MGrid s o -> ST s (Either String r) }

{-
instance Functor (GridOp o) where
    fmap f m = GridOpCtor $ \mg -> f <$> gridOpFn m mg
-}

instance Functor (GridOp o) where
    fmap f (GridOpCtor fn) = GridOpCtor $ \mg -> do
        result <- fn mg
        return $ f <$> result

instance Applicative (GridOp o) where
    pure r = GridOpCtor $ \_ -> return (Right r)
    l <*> r = GridOpCtor $ \mg -> do
        fn <- gridOpFn l mg
        val <- gridOpFn r mg
        return $ fn <*> val

instance Monad (GridOp o) where
    fail str = GridOpCtor $ \_ -> return (Left str)
    return r = GridOpCtor $ \_ -> return (Right r)
    m >>= fn = GridOpCtor $ \mg -> do
        result <- gridOpFn m mg
        case result of
            Left err -> return (Left err)
            Right r -> gridOpFn (fn r) mg


getM :: Pos -> GridOp o o
getM (x, y) = GridOpCtor $ \mg -> Right <$> MV.read (_vectorM mg) (y * widthM mg + x)


setM :: Pos -> o -> GridOp o ()
setM (x, y) o = GridOpCtor $ \mg -> Right <$> MV.write (_vectorM mg) (y * widthM mg + x) o

setiM :: Int -> o -> GridOp o ()
setiM i o = GridOpCtor $ \mg -> Right <$> MV.write (_vectorM mg) i o


sizeM :: GridOp o (Int, Int)
sizeM = GridOpCtor $ \mg -> return . Right . _sizeM $ mg

containsM :: Pos -> GridOp o Bool
containsM (x, y) = do
    (w, h) <- sizeM
    return $ not (x < 0 || y < 0 || x >= w || y >= h)

fillM :: o -> GridOp o ()
fillM o = do
    (w, h) <- sizeM
    forM_ [0..h-1] $ \y ->
        forM_ [0..w-1] $ \x -> setM (x, y) o

foldGridM :: (a -> Pos -> o -> a) -> a -> GridOp o a
foldGridM fn a = GridOpCtor $ \mg -> do
    v <- V.freeze (_vectorM mg)
    return . Right $ V.ifoldl (foldFn $ _sizeM mg) a v
    where
        foldFn (w, _) acc i = fn acc (i `mod` w, i `div` w)

runGridOp :: Grid o -> GridOp o r -> (Either String r, Grid o)
runGridOp g op = runST $ do
    mv <- V.thaw (_vector g)
    r <- gridOpFn op (MGridCtor (_size g) mv)
    v <- V.freeze mv
    return (r, g { _vector = v })

evalGridOp :: forall o r. Grid o -> GridOp o r -> Either String r
evalGridOp g op = fst (runGridOp g op)

execGridOp :: forall o r. Grid o -> GridOp o r -> Grid o
execGridOp g op = snd (runGridOp g op)


