-----------------------------------------
--
-- いろいろな素数
--
-----------------------------------------


import MyModule.Primes
import MyModule.Utility

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


main = print $ take 10 $ primeKTuples [6]
