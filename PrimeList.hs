-------------------------------------------------------------
--
-- Module : MyModule.PrimeList
-- Coding : Little Schemer
--
-- エラトステネスの篩による素数リスト Data.Vector 版
--
-------------------------------------------------------------

module MyModule.PrimeList (primeList) where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Monad.ST (runST)
import  Control.Monad (when)


-- エラトステネスの篩
sieve :: Int -> U.Vector Bool
sieve n = runST $ do
  mVec <- UM.replicate (n + 1) True
  mapM_ (setFalse mVec) (0 : 1 : [4, 6 .. n])
  mapM_ (loop mVec) [3, 5 .. floor $ sqrt $ fromIntegral n]
  U.unsafeFreeze mVec
    where
      setFalse vec i = UM.unsafeWrite vec i False
      loop vec i = do
        v <- UM.unsafeRead vec i
        when v $ mapM_ (setFalse vec) [i * i, i * (i + 2) .. n]

-- エラトステネスの篩による素数リスト
primeList :: Int -> [Int]
primeList n = U.toList $ U.elemIndices True $ sieve n

