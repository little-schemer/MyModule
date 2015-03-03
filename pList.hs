-------------------------------------------------------------
--
-- Module : My.PrimeList.hs
-- Coding : Tsumuji
--
-- エラトステネスの篩による素数リスト
--
-------------------------------------------------------------



import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Base (unsafeRead, unsafeWrite)
import Control.Monad

-- エラトステネスの篩
sieve :: Int -> UArray Int Bool
sieve n = runSTUArray $ do
  arr <- newArray (0, n) True
  mapM_ (setFalse arr) $ [0, 1] ++ [4, 6 .. n]
  mapM_ (loop arr) [3, 5 .. floor $ sqrt $ fromIntegral n]
  return arr
    where
      loop arr k = do
        v <- unsafeRead arr k
        when v $ mapM_ (setFalse arr) [k * k, k * (k + 2) .. n]
      setFalse arr k = unsafeWrite arr k False

-- エラトステネスの篩による素数リスト
primeList :: Int -> [Int]
primeList n = [p | (p, bool) <- assocs $ sieve n, bool]

main = print $ sum $ map fromIntegral $ primeList 10000000
-- main = print $ last $ primeList 100000000
