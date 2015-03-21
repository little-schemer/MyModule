-------------------------------------------------------------
--
-- 素数関連の関数群
--
-- Module : MyModule.Primes
-- Coding : Little Schemer
--
-------------------------------------------------------------

module MyModule.Primes where

import Data.List (group, intercalate)


-----------------------------------------
-- 素数関連
-----------------------------------------

--
-- 素数列
--
-- http://d.hatena.ne.jp/notogawa/20110114/1295006865 を参考に
-- gcd の使い方が秀逸
--
primes :: Integral a => [a]
primes = map fromIntegral $ [2, 3] ++ primes'
    where
      primes' = [5] ++ f 5 7 primes'
      f m s (p : ps) = [n | n <- ns, gcd m n == 1] ++ f (m * p) (p * p) ps
          where ns = concat [[x, x + 4] | x <- [s, s + 6 .. p * p - 2]]

-- memo :
--  num1 = 2 : 3 : [x + y | x <- [6, 12 ..], y <- [-1, 1]]
--  num2 = 2 : 3 : concat [[6 * x -1, 6 * x + 1] | x <- [1 ..]]
--  だと、num2 の方が速い。

--
-- 素数判定 (試し割り法)
--
-- http://itchyny.hatenablog.com/entry/2014/10/01/100000 を参考に
--
isPrime :: Integral a => a -> Bool
isPrime n = n > 1 && foldr f True primes
  where f p b = (p * p > n) || (rem n p /= 0 && b)



-----------------------------------------
-- 素因数分解関連
-----------------------------------------

--
-- 素因数分解 (試し割り法)
--
-- ex : factorize  12 = [(2,2),(3,1)]
-- ex : factorize' 12 = [2,2,3]
--
factorize :: Integral a => a -> [(a, Int)]
factorize n = [(x, length xs) | xs@(x : _) <- group $ factorize' n]

factorize' :: Integral a => a -> [a]
factorize' 1 = []
factorize' n = loop n primes
  where
    loop n ps@(p : ps')
      | p * p > n    = [n]
      | rem n p == 0 = p : loop (div n p) ps
      | otherwise    = loop n ps'

--
-- 素因数分解した結果の表示
--
showFactorize :: (Integral a, Show a) => a -> String
showFactorize n = intercalate " * " [f p i | (p, i) <- factorize n]
  where f p i = if i == 1 then show p else show p ++ "^" ++ show i
