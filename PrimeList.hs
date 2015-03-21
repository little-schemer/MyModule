-------------------------------------------------------------
--
-- エラトステネスの篩による素数リスト Data.Vector 版
--
-- Module : MyModule.PrimeList
-- Coding : Little Schemer
--
-------------------------------------------------------------

module MyModule.PrimeList (primeList) where

import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M
import Control.Monad.ST (runST)
import Control.Monad    (when)

--
-- エラトステネスの篩による素数リスト
--
-- * ex : primeList 20  =>  [2,3,5,7,11,13,17,19]
--
primeList :: Int -> [Int]
primeList n = U.toList $ U.elemIndices True $ sieve n

-- エラトステネスの篩
sieve :: Int -> U.Vector Bool
sieve n = runST $ do
  mVec <- M.replicate (n + 1) True
  mapM_ (setFalse mVec) (0 : 1 : [4, 6 .. n])
  mapM_ (loop mVec) [3, 5 .. floor $ sqrt $ fromIntegral n]
  U.unsafeFreeze mVec
    where
      setFalse vec i = M.unsafeWrite vec i False
      loop vec i = do
        v <- M.unsafeRead vec i
        when v $ mapM_ (setFalse vec) [i * i, i * (i + 2) .. n]
