-------------------------------------------------------------
--
-- Module : MyModule.PrimeList
-- Coding : Little Schemer
--
-- �G���g�X�e�l�X��⿂ɂ��f�����X�g
--
-------------------------------------------------------------

module MyModule.PrimeList (primeList) where

import Data.Array.ST
import Data.Array.Unboxed
import Data.Array.Base (unsafeRead, unsafeWrite)
import Control.Monad



-- �G���g�X�e�l�X���
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

-- �G���g�X�e�l�X��⿂ɂ��f�����X�g
primeList :: Int -> [Int]
primeList n = [p | (p, bool) <- assocs $ sieve n, bool]
