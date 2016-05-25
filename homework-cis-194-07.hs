import Debug.Trace

import Prelude hiding (mapM)
-- import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V

-- Exercise 1

liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f ma = ma >>= return . f
-- liftM = fmap

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v =
  do
    vi <- v !? i
    vj <- v !? j
    return $ v // [(i, vj)] // [(j, vi)]

-- Exercise 2

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as =
    if null as then return [] else
      do
        b <- f $ head as
        bs <- mapM f $ tail as
        return (b : bs)

getElts :: [Int] -> Vector a -> Maybe [a]
getElts as v = mapM (\ a -> v !? a) as

-- Exercise 3

-- type Rnd a = Rand StdGen a

-- evalRandIO :: Rnd a -> IO a
-- evalRandIO r = return $ liftM r

-- getRandom :: Random a => Rnd a
-- getRandom r = 
-- getRandomR :: Random a => (a, a) -> Rnd a

-- randomElt :: Vector a -> Rnd (Maybe a)
-- randomElt as = 