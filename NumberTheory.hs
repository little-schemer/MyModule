-------------------------------------------------------------
--
-- 数学関連の関数群
--
-- Module : MyModule.NumberTheory
-- Coding : Little Schemer
--
-------------------------------------------------------------

module MyModule.NumberTheory where

import Data.List        (sort)
import Data.Ratio
import Data.Maybe
import MyModule.Primes  (factorize)
import MyModule.Utility (integralToList)


-----------------------------------------
-- 冪乗法
-----------------------------------------
-- 『素因数分解と素数判定』 p.29 参照
--
-- * 関数 f は Monoid 則を満していること。
-- * e の初期値 : 関数 f の単位元
--
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
-- 数列関連
-----------------------------------------

--
-- Fibonacci 数列のリスト
--
-- * "seq" を使うと速くなる
--
fibonacciList :: [Integer]
fibonacciList = fib' 0 1
    where fib' a b = seq a $ a : fib' b (a + b)

--
-- Fibonacci 数
--
-- * 「Gosper & Salamin のアイデア」を使用。
-- * from 『フィボナッチ数とリュカ数の計算法』
--
fibonacci :: Int -> Integer
fibonacci 0 = 0
fibonacci n = fst $ power calc (0, 1) (1, 0) n
  where calc (a, b) (c, d) = (a * (c + d) + b * c, a * c + b * d)

--
-- Fibonacci 数
--
-- *「ビネの公式」を使った方法
-- * 参考 :
--   http://labs.timedia.co.jp/2012/11/fibonacci-general-term-using-rational.html
-- * (1 / 2 + √5 / 2) ^ n - (1 / 2 - √5 / 2) ^ n を計算すると、有理数
--   項は打ち消しあって、√5 の項のみが残る。
--
fibonacci2 :: Int -> Integer
fibonacci2 0 = 0
fibonacci2 n = truncate . (* 2) . snd $ power f (1, 0) (1 % 2, 1 % 2) n
  where f (a1, b1) (a2, b2) = (a1 * a2 + 5 * b1 * b2, a1 * b2 + a2 * b1)

--
-- Lucas 数
--
lucasList :: [Integer]
lucasList = lucas' 2 1
  where lucas' a b = seq a $ a : lucas' b (a + b)

--
-- 多角数のリスト
--
-- * "0" から始まるので注意 !!
--
-- ex : polyNumList 3 => [0,1,3,6,10,15,21,28,36,45,55 ..]
--
polyNumList :: Integral a => a -> [a]
polyNumList n = scanl (+) 0 [1, n - 1 ..]

--
-- 多角数の一般項
--
-- * from Wikipedia
--
-- ex : map (polyNum 3) [1 .. 5] => [1,3,6,10,15]
--
polyNum :: Integral a => a -> a -> a
polyNum p n = div (n * ((p - 2) * n - (p - 4))) 2



-----------------------------------------
-- 約数関連
-----------------------------------------

--
-- 約数
--
divisor :: Integral a => a -> [a]
divisor n = sort $ foldr f [1] ns
    where
      ns = [map (p ^) [0 .. i] | (p, i) <- factorize n]
      f ns1 ns2 = [x * y | x <- ns1, y <- ns2]

--
-- 約数の個数
--
numberOfDivisors :: Integral a => a -> Int
numberOfDivisors n = product [i + 1 | (_, i) <- factorize n]

--
-- 約数の和
--
sumOfDivisors :: Integral a => a -> a
sumOfDivisors n = product [f p i | (p, i) <- factorize n]
    where f p i = div (p ^ (i + 1) - 1) (p - 1)

--
-- 完全数か ?
--
isPerfect :: Integral a => a -> Bool
isPerfect n = n + n == sumOfDivisors n

--
-- 過剰数か ?
--
isAbundant :: Integral a => a -> Bool
isAbundant n = n + n < sumOfDivisors n

--
-- 不足数か ?
--
isDeficient :: Integral a => a -> Bool
isDeficient n = n + n > sumOfDivisors n

--
-- 友愛数のもう片方を探す
--
-- ex : findAmicableNumber 220 => Jus 284
-- ex : findAmicableNumber 221 => Nothing
--
findAmicableNumber :: Integral a => a -> Maybe a
findAmicableNumber x
  | (x /= y) && (sumOfDivisors y - y == x) = Just y
  | otherwise = Nothing
  where y = sumOfDivisors x - x

--
-- 友愛数のペアのうち、小さい値が引数の範囲内にあるものを返す。
--
-- ex : amicablePairs [2 .. 1200] => [(220,284),(1184,1210)]
--
amicablePairs :: Integral a => [a] -> [(a, a)]
amicablePairs xs = [(x, fromJust y) | x <- xs,
                    let y = findAmicableNumber x, isJust y, Just x < y]



-----------------------------------------
-- ピタゴラス数
-----------------------------------------

--
-- ピタゴラス数
--
-- * a + b + c = n, a^2 + b^2 = c^2, a < b < c の組を探す
--
-- 1. a + a + a < n よって a <= div n 3
-- 2. a^2 + b^2 = (n - a - b)^2 変形すると、
--    b = n / 2 - (a * n) / (2 * (n - a))
-- 3. a, b ともに整数なら、n は必ず偶数になるので
--    （既約ピタゴラス数の場合、a と b はどちらかは偶数でもう一方は奇数、
--     さらに c は奇数になることが判っている）
--    i ) n/2 は整数。
--    ii) 従って、(a * n) / (2 * (n - a)) も整数。
--
pythagoreanNums :: Integral a => a -> [(a, a, a)]
pythagoreanNums n
    | odd n     = []
    | otherwise = [(a, b, n - a - b) | a <- as, let b = calc a, a < b]
    where
      as = [a | a <- [1 .. div n 3], rem (a * n) (2 * (n - a)) == 0]
      calc a = div (n * (n - 2 * a)) (2 * (n - a))

--
-- 既約ピタゴラス数
--
primitivePythagoreanNums :: Integral a => a -> [(a, a, a)]
primitivePythagoreanNums n =
    [(a, b, c) | (a, b, c) <- pythagoreanNums n, gcd a b == 1]



-----------------------------------------
-- 順列と組み合わせ
-----------------------------------------

--
-- 順列の数
--
-- ex : permutationSize 5 3 => 60
--
permutationSize :: Integral a => a -> a -> a
permutationSize = fallingFactorial

--
-- 組み合わせの数
--
-- ex : combinationSize 5 3 => 10
--
combinationSize :: Integral a => a -> a -> a
combinationSize m n = div (fallingFactorial m n) (factorial n)



-----------------------------------------
-- その他
-----------------------------------------

--
-- 平方根の整数部分
--
isqrt :: Integral a => a -> a
isqrt = truncate . sqrt . fromIntegral

--
-- 整数か？(小数点以下は 0 か？)
--
isInteger :: RealFrac a => a -> Bool
isInteger = (== 0) . snd . properFraction

--
-- 下降階乗冪
--
fallingFactorial :: Integral a => a -> a -> a
fallingFactorial m n = product [m - n + 1 .. m]

--
-- 階乗
--
factorial :: Integral a => a -> a
factorial m = product [2 .. m]

--
-- 整数の桁数を求める
--
digits ::  (Integral a, Show a) => a -> Int
digits  = length . show



-----------------------------------------
-- オイラーのφ関数
-----------------------------------------
--
-- * 正の整数 n に対して、1 から n までの自然数のうち n と互いに素な
--   ものの個数（1 と n は互いに素と考える）
--
-- * factorize n => [(a, i), (b, j), (c, l) ...] の時、
--   phi n = n * (1 - 1 / a) * (1 - 1 / b) * (1 - 1 / c) * ...
--         = n * (a - 1) / a * (b - 1) / b * (c - 1) / c * ...
--         = a ^ (i - 1) * (a - 1) * b ^ (j - 1) * (b - 1) * ...
--
phi :: Integral a => a -> a
phi n = product [p^(i - 1) * (p - 1) | (p, i) <- factorize n]



-----------------------------------------
-- メビウス関数
-----------------------------------------
--
-- * 定義（ただし 1 は 0 個の素因数を持つと考える）：
--   μ(n) = 0      （n が平方因子を持つ時）
--   μ(n) = (-1)^k （n が相異なる k 個の素因数に分解されるとき）
--
mobius :: Integral a => a -> a
mobius n
  | any (>= 2) is    =  0
  | even (length is) =  1
  | otherwise        = -1
  where is = [i | (_, i) <- factorize n]


-----------------------------------------
-- 2 進表現のビット数を返す
-----------------------------------------
binarySize :: Integral a => a -> Int
binarySize = length . integralToList 2


-----------------------------------------
-- ペル方程式
-----------------------------------------
--
-- * "x^2 - d * y^ = 1" の解 (x, y) (x > 1, y > 1) を小さい順に返す。
--
-- * from http://www004.upp.so-net.ne.jp/s_honma/pell/pell.htm
--
pell'sEquation :: Integer -> [(Integer, Integer)]
pell'sEquation d = iterate (f (x, y)) (x, y)
  where
    f (a1, b1) (a2, b2) = (a1 * a2 + d * b1 * b2, a1 * b2 + a2 * b1)
    (x, y) = loop 0 1 k0 1 0
      where
        k0 = (truncate . sqrt . fromIntegral) d
        loop g1 h1 k1 y0 y1
          | g1 == g2  = g $ f (x1, y1) (x1, y1)
          | h1 == h2  = let ns = g $ f (x1, y1) (x2, y2) in f ns ns
          | otherwise = loop g2 h2 k2 y1 y2
          where
            g2 = k1 * h1 - g1
            h2 = div (d - g2^2) h1
            k2 = div (k0 + g2) h2
            y2 = y0 + k1 * y1
            x1 = g1 * y1 + h1 * y0
            x2 = g2 * y2 + h2 * y1
            g (x, y) = (div x h1, div y h1)
