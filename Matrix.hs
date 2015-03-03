-------------------------------------------------------------
--
-- Module : Matrix.hs
-- Coding : Tsumuji
--
-- ベクトル及び行列の計算
--
-------------------------------------------------------------

module My.Matrix where

import Data.List  (transpose)
import My.Primes  (power)
import My.Utility (deleteAt, splits)


-----------------------------------------
--  ベクトルの演算
-----------------------------------------
--
--  + ベクトルは数値のリストの形で表す。
--  + ex : 3次元のベクトル [1,2,3]
--

type Vector a = [a]


--
-- ベクトルの m 番目の要素
--
-- (通常のリストと異り、1 から数え始める)
--
vectorRef :: Num a => Int -> Vector a -> a
vectorRef m xs = xs !! (m - 1)

--
-- Scalar * Vector
--
(*/) :: Num a => a -> Vector a -> Vector a
(*/) n xs = map (* n) xs
infixl 7 */

--
-- Vector + Vector
--
(/+/) :: Num a => Vector a -> Vector a -> Vector a
(/+/) xs ys = zipWith (+) xs ys
infixl 6 /+/

--
-- Vector - Vector
--
(/-/) :: Num a => Vector a -> Vector a -> Vector a
(/-/) xs ys = zipWith (-) xs ys
infixl 6 /-/

--
-- Vector * Vector (inner product)
--
(/*/) :: Num a => Vector a -> Vector a -> a
(/*/) xs ys = sum $ zipWith (*) xs ys
infixl 7 /*/

--
-- Vector の絶対値
--
vectorAbs :: Floating a => Vector a -> a
vectorAbs xs = sqrt (xs /*/ xs)

--
-- n 次元のゼロベクトル
--
zeroVector :: Num a => Int -> Vector a
zeroVector n = replicate n 0



-----------------------------------------
-- 行列の演算
-----------------------------------------
--
-- + 行列は行ベクトルのリストの形で表す。
-- + ex : 2行3列の行列 [[1,2,3],[4,5,6]]
-- + 行と列の整合性は調べていないので注意!!
--

type Matrix a = [Vector a]


--
-- リストから m 行 n 列の行列を作る
--
makeMatrix :: Num a => (Int, Int) -> [a] -> Matrix a
makeMatrix (m, n) xs = check $ splits n xs
  where
    check ma
      | (length ma == m) && (length (last ma) == n) = ma
      | otherwise = error "Wrong Matrix !"

--
-- m 行 n 列の行列かを調べる
--
checkMatrix :: Num a => (Int, Int) -> Matrix a -> Bool
checkMatrix (m, n) ma = (length ma == m) && (and [length xs == n | xs <- ma])

--
-- 行列の m 行目 n 列目の要素
--
-- ( 通常のリストと異り 1 から数え始める )
--
matrixRef :: Num a => (Int, Int) -> Matrix a -> a
matrixRef (m, n) xss = (xss !! (m - 1)) !! (n - 1)

--
-- Scalar * Matrix
--
(*|) :: Num a => a -> Matrix a -> Matrix a
(*|) n xss = map (n */) xss
infixl 7 *|

--
-- Matrix + Matrix
--
(|+|) :: Num a => Matrix a -> Matrix a -> Matrix a
(|+|) xss yss = zipWith (/+/) xss yss
infixl 6 |+|

--
-- Matrix - Matrix
--
(|-|) :: Num a => Matrix a -> Matrix a -> Matrix a
(|-|) xss yss = zipWith (/-/) xss yss
infixl 6 |-|

--
-- Matrix * Matrix
--
(|*|) :: Num a => Matrix a -> Matrix a -> Matrix a
(|*|) xss yss = [[xs /*/ ys | ys <- yss'] | xs <- xss]
    where yss' = transpose yss
infixl 7 |*|

--
-- m 行 n 列のゼロ行列
--
zeroMatrix :: Num a => (Int, Int) -> Matrix a
zeroMatrix (m, n) = replicate m $ zeroVector n

--
-- n 次の単位行列
--
unitMatrix :: Num a => Int -> Matrix a
unitMatrix n = map f [0 .. (n - 1)]
  where f m = take n $ replicate m 0 ++ (1 : repeat 0)

--
-- Matrix ^ n （正方行列のみ可）
--
(|^) :: Num a => Matrix a -> Int -> Matrix a
(|^) xss n = power (|*|) (unitMatrix (length xss)) xss n
infixr 8 |^

--
-- 行列式
--
-- + 再帰を繰り返すので遅い。LU 分解を使った高速な方法があるらしい。
--
det :: Num a => Matrix a -> a
det [[a]] = a
det [[a, b], [c, d]] = a * d - b * c
det [[a, b, c], [d, e, f], [g, h, i]] =
  a * e * i + c * d * h + b * f * g - c * e * g - a * f * h - b * d * i
det (m : ms) = sum $ zipWith (*) m [cofactor1 j ms | j <- [1 ..]]
  where cofactor1 j ms = if odd j then x else negate x
          where x = det $ map (deleteAt (j - 1)) ms


-- from http://d.hatena.ne.jp/dwarfjay/20140409/1397030010
-- pmat :: Int -> [[a]] -> [[a]]
-- pmat n d = [tail x | x <- left ++ tail right]
--     where (left, right) = splitAt n d

-- det2 :: (Num a) => [[a]] -> a
-- det2 [[x]] = x
-- det2 [[a, b], [c, d]] = a * d - b * c
-- det2 [[a, b, c], [d, e, f], [g, h, i]] =
--   a * e * i + c * d * h + b * f * g - c * e * g - a * f * h - b * d * i
-- det2 d = sum [s * head x * det2 (pmat i d) | (s, i, x) <- zip3 (cycle [1, -1]) [0..] d]

--
-- (i, j) 余因子
--
cofactor :: Num a => (Int, Int) -> Matrix a -> a
cofactor (i, j) ma = if odd (i + j) then negate x else x
  where x = det $ map (deleteAt (j - 1)) $ deleteAt (i - 1) ma

--
-- Fibonacci 数
--
-- * 行列を使用
--
fibonacci :: Int -> Integer
fibonacci n = matrixRef (1, 2) $ [[1,1],[1,0]] |^ n
