-------------------------------------------------------------
--
-- 篩系
--
-- Module : MyModule.Sieve
-- Coding : Little Schemer
--
-------------------------------------------------------------

module MyModule.Sieve where


import qualified Data.Vector                 as V
import qualified Data.Vector.Mutable         as VM
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Monad.ST
import Control.Monad
import Data.List


--
-- エラトステネスの篩による素数リスト
--
-- * ex : primeList 20  =>  [2,3,5,7,11,13,17,19]
--
primeList :: Int -> [Int]
primeList n = 2 : (map indexToValue $ U.toList $ U.elemIndices True $ sieve)
  where
    indexToValue i = 2 * i + 3
    valueToIndex v = div (v - 3) 2
    lastIndex      = valueToIndex n

    sieve = runST $ do
      mVec <- UM.replicate (lastIndex + 1) True
      mapM_ (loop mVec) [0 .. valueToIndex (floor $ sqrt $ fromIntegral n)]
      U.unsafeFreeze mVec

    loop vec i = do
      v <- UM.unsafeRead vec i
      when v $ do
        let (s, d) = (2 * i * (i + 3) + 3, indexToValue i)
        mapM_ setFalse [s, s + d .. lastIndex]
          where setFalse i = UM.unsafeWrite vec i False


--
-- 1 〜 n までの約数のリスト
--
-- * ex : divisorsList 5  =>  [[1],[1,2],[1,3],[1,2,4],[1,5]]
--
divisorsList :: Int -> [[Int]]
divisorsList n = tail $ V.toList $ sieve
  where
    sieve = runST $ do
      mVec <- VM.replicate (n + 1) []
      mapM_ (setDivs mVec) [n, n - 1 .. 1]
      V.unsafeFreeze mVec

    setDivs vec i  = mapM_ (setNum i) [i, i + i .. n]
      where
        setNum n i = do
          lst <- VM.unsafeRead vec i
          VM.unsafeWrite vec i (n : lst)


--
-- 1 〜 n までの素因数分解のリスト
--
-- ex : factorsList' 5  =>  [[],[2],[3],[2,2],[5]]
-- ex : factorsList  5  =>  [[],[(2,1)],[(3,1)],[(2,2)],[(5,1)]]
--
factorsList :: Int -> [[(Int, Int)]]
factorsList n = map f $ factorsList' n
  where f xs = [(p, length ps) | ps@(p : _) <- group xs]

factorsList' :: Int -> [[Int]]
factorsList' n = tail $ map reverse $  V.toList $ sieve
  where
    sieve = runST $ do
      vec <- VM.replicate (n + 1) []
      mapM_ (consP vec) [2 .. n]
      V.unsafeFreeze vec

    consP vec p = do
      v <- VM.unsafeRead vec p
      when (null v) $ mapM_ f $ takeWhile (<= n) $ iterate (* p) p
        where
          f i = mapM_ g [i, 2 * i .. n]
          g i = do
            ps <- VM.unsafeRead vec i
            VM.unsafeWrite vec i (p : ps)


--
-- 1 〜 n までのオイラーのφ関数のリスト
--
-- ex : phiList 10  =>  [1,1,2,2,4,2,6,4,6,4]
--
phiList :: Int -> [Int]
phiList n = map f $ factorsList n
  where f xs = product [p^(i - 1) * (p - 1) | (p, i) <- xs]
