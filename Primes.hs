-------------------------------------------------------------
--
-- Module : Primes.hs
-- Coding : Tsumuji
--
-- 素数関連の関数群
--
-------------------------------------------------------------

module My.Primes where

import Data.List  (group, intercalate)
import My.Utility (reverseInt)


-----------------------------------------
-- 冪乗法
-----------------------------------------
-- 『素因数分解と素数判定』 p.29 参照
--
-- e の初期値 : 関数 f の単位元
-- ex : 2^100 == power (*) 1 2 100
--
power :: Integral a => (t -> t -> t) -> t -> t -> a -> t
power _ e _ 0 = e
power f e a n = power f (if odd n then f a e else e) (f a a) (div n 2)

--
-- a^n (mod m)
--
powerMod :: (Integral a, Integral t) => t -> a -> t -> t
powerMod a n m = power (\x y -> mod (x * y) m) 1 a n



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
-- from http://itchyny.hatenablog.com/entry/2014/10/01/100000
--
isPrime :: Integral a => a -> Bool
isPrime n = n > 1 && foldr f True primes
  where f p r = (p * p > n) || (rem n p /= 0 && r)



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
showFactorize n = intercalate " * " lst
  where lst = [show p ++ "^" ++ show i | (p, i) <- factorize n]

--
-- factorize のテスト用
--
factorizeTest :: Integral a => [(a, Int)] -> a
factorizeTest xs = product [f ^ i | (f, i) <- xs]



-----------------------------------------
-- メルセンヌ数
-----------------------------------------
mersenneNumber :: Integral a => a -> a
mersenneNumber n = 2 ^ n - 1

-- メルセンヌ数の素数判定 (リュカ・レーマーのアルゴリズム)
-- 2 ^ n - 1 は素数か？ (n : 奇数)
isMersennePrime :: Integral a => a -> Bool
isMersennePrime n = foldr check 4 [3 .. n] == 0
  where
    m = 2 ^ n - 1
    check x y = mod (y * y - 2) m

-- メルセンヌ素数
mersennePrimes :: Integral a => [(a, a)]
mersennePrimes = (2, 3) : [(p, mersenneNumber p) | p <- primes, isMersennePrime p]




-----------------------------------------
-- ソフィー・ジェルマン素数と安全素数
-----------------------------------------
--
-- * p と 2p + 1 がともに素数である時、p を「ソフィー・ジェルマン素数」
--   といい、2p + 1 を「安全素数」という。
--

--
-- ソフィー・ジェルマン素数と安全素数のペアのリストを返す
--
sgAndSafePrimes :: Integral a => [(a, a)]
sgAndSafePrimes = loop primes
  where
    loop (p : ps)
      | p' == (head $ dropWhile (< p') ps) = (p, p') : loop ps
      | otherwise                          = loop ps
      where p'  = 2 * p + 1

--
-- ソフィー・ジェルマン素数か？
--
isSGPrime :: Integral a => a -> Bool
isSGPrime n = isPrime n && isPrime (2 * n + 1)

--
-- 安全素数か？
--
isSafePrime :: Integral a => a -> Bool
isSafePrime 2 = False
isSafePrime n = isPrime n && isPrime (div (n - 1) 2)



-----------------------------------------
-- エマープ (emirp)
-----------------------------------------
--
-- * 素数でありかつ逆から数字を読んでも(元の数とは違う)素数になる自数の
--   こと。
--

--
-- エマープか？
--
isEmirp :: Integral a => a -> Bool
isEmirp n = isPrime n && isEmirp' n

--
-- エマープのリストを返す
--
emirps :: Integral a => [a]
emirps = filter isEmirp' primes

--
-- 補助関数
--
isEmirp' :: Integral a => a -> Bool
isEmirp' n = n /= n' && isPrime n'
  where n' = reverseInt n



-----------------------------------------
-- 回文素数
-----------------------------------------
--
--  * 回文素数とは、位取り記数法による表記が（通常は十進法で）回文数に
--    なっている素数のことである。
--

--
-- 回文素数か？
--
isPalindromicPrime :: Integral a => a -> Bool
isPalindromicPrime n = isPrime n && isPalindromicPrime' n

--
-- 回文素数のリスト
--
palindromicPrimes :: Integral a => [a]
palindromicPrimes = filter isPalindromicPrime' primes

--
-- 補助関数
--
isPalindromicPrime' n = n == n' && isPrime n'
  where n' = reverseInt n



-----------------------------------------
-- Prime k-tuple
-----------------------------------------

--
-- Prime k-tuple の一般化
--
-- 双子素数     : primeKTuples [2]
-- いとこ素数   : primeKTuples [4]
-- セクシー素数 : primeKTuples [6]
--
-- 三つ子素数(1) : primeKTuples [2, 6]
-- 三つ子素数(2) : primeKTuples [4, 6]
-- セクシー素数の三つ組 :
--     filter (\(p : _) -> (not . isPrime) (p + 18)) $ primeKTuples [6, 12]
--
-- 四つ子素数 : primeKTuples [2, 6, 8]
-- セクシー素数の四つ組 : primeKTuples [6, 12, 18]
--
-- 五つ子素数(1) : primeKTuples [2, 6, 8, 12]
-- 五つ子素数(2) : primeKTuples [4, 6, 10, 12]
--
-- 六つ子素数 : primeKTuples [4, 6, 10, 12, 16]
--
primeKTuples :: Integral a => [a] -> [[a]]
primeKTuples ds = loop primes
  where
    loop (p : ps)
      | checkDiff = (p : ps') : loop ps
      | otherwise = loop ps
      where
        checkDiff = map (subtract p) ps' == ds
        ps' = map (head . (\d -> dropWhile (< p + d) ps)) ds

--
-- merge を使用すると「三つ子素数」や「五つ子素数」を一つにまとめられる。
-- 三つ子素数 : merge (primeKTuples [2, 6]) (primeKTuples [4, 6])
-- 五つ子素数 : merge (primeKTuples [2, 6, 8, 12]) (primeKTuples [4, 6, 10, 12])
--
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs@(x : xs') ys@(y : ys')
  | x <= y = x : merge xs' ys
  | otherwise = y : merge xs ys'
