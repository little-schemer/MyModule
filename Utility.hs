-------------------------------------------------------------
--
-- ユーティリティ関数群
--
-- Module : MyModule.Utility
-- Coding : Little Schemer
--
-------------------------------------------------------------

module MyModule.Utility where

import Data.List (foldl', sort, tails)
import Data.Char (intToDigit)
import Control.Monad (replicateM)


-----------------------------------------
-- 整数とリストの変換
-----------------------------------------

--
-- 整数をリストに変換
--
-- ex : integralToList 10 123 => [1,2,3]
-- ex : integralToList 2 123  => [1,1,1,1,0,1,1]
--
integralToList :: Integral a => a -> a -> [a]
integralToList radix n = loop n []
    where
      loop m prd
        | m < radix = m : prd
        | otherwise = loop (div m radix) (rem m radix : prd)

--
-- 整数をリストに変換 (十進法限定)
--
dexToList :: Integral a => a -> [a]
dexToList = integralToList 10

--
-- リストを整数に変換
--
-- ex : listToIntegral 10 [1,2,3]        => 123
-- ex : listToIntegral 2 [1,1,1,1,0,1,1] => 123
--
listToIntegral :: Integral a => a -> [a] -> a
listToIntegral radix ns = foldl' ((+) . (* radix)) 0 ns

--
-- リストを整数に変換 (十進法限定)
--
listToDex :: Integral a => [a] -> a
listToDex = listToIntegral 10



----------------------------------------
-- 回文数関連
-----------------------------------------

--
-- 整数を反転させる
--
-- ex : reverseInt 123  =>  321
--
reverseInt :: Integral a => a -> a
reverseInt = listToDex . reverse . dexToList

--
-- 回文リストか？
--
isPalindromicList :: Eq a => [a] -> Bool
isPalindromicList ns = ns == reverse ns

--
-- 回文数か？
--
isPalindromic :: Integral a => a -> Bool
isPalindromic = isPalindromicList . dexToList

--
-- 奇数桁の回文数を作る
--
oddPalNum :: Integral a => a -> a
oddPalNum n = listToDex $ ns ++ (tail . reverse) ns
    where ns = dexToList n

--
-- 偶数桁の回文数を作る
--
evenPalNum :: Integral a => a -> a
evenPalNum n = listToDex $ ns ++ reverse ns
    where ns = dexToList n



-----------------------------------------
-- Pandigital 数関連
-----------------------------------------

--
-- Pandigital な String か？
--
isPandigitalStr :: String -> Bool
isPandigitalStr str = and $ zipWith (==) (sort str) "1234567890"

--
-- Pandigital な数か？
--
isPandigitalNumber :: Int -> Bool
isPandigitalNumber = isPandigitalStr . show

--
-- Pandigital な List か？
--
isPandigitalList :: [Int] -> Bool
isPandigitalList = isPandigitalStr . map intToDigit



-----------------------------------------
-- 順列と組み合わせ
-----------------------------------------

--
-- 順列 ( 辞書順 )
--
-- + Data.List に "permutations" が存在するが返り値が辞書順ではない
-- ex : permutation [1..3] 2 => [[1,2],[1,3],[2,1],[2,3],[3,1],[3,2]]
--
permutation :: [a] -> Int -> [[a]]
permutation ns n = perm n (length ns - 1) [(ns, [])]
    where
      perm 0 _ xs = [a | (_, a) <- xs]
      perm c n xs = perm (c - 1) (n - 1) $ concatMap (f n) xs
      f n (xs, ys) = [(as ++ bs, ys ++ [b]) |
                      i <- [0 .. n], let (as, b : bs) = splitAt i xs]

--
-- 重複順列 ( 辞書順 )
--
-- ex : repPermutation [1..3] 2
--         => [[1,1],[1,2],[1,3],[2,1],[2,2],[2,3],[3,1],[3,2],[3,3]]
--
repPermutation :: [a] -> Int -> [[a]]
repPermutation ns n = replicateM n ns

-- repPermutation :: [a] -> Int -> [[a]]
-- repPermutation ns n = perm n [[]]
--     where
--       perm 0 xs = xs
--       perm c xs = perm (c - 1) $ concatMap f xs
--       f xs = [xs ++ [x] | x <- ns]

--
-- 組み合わせ ( 辞書順 )
--
-- ex : combination [1 .. 4] 2 => [[1,2],[1,3],[1,4],[2,3],[2,4],[3,4]]
--
combination :: [a] -> Int -> [[a]]
combination ns n = comb n [(ns, [])]
    where
      comb 0 xs = [a | (_, a) <- xs]
      comb c xs = comb (c - 1) $ concatMap f xs
      f (xs, ys) = map (\(a : as) -> (as, ys ++ [a])) $ init $ tails xs

--
-- 重複組み合わせ (repeated combination)
--
-- ex : repComb [0..2] 2  =>  [[0,0],[0,1],[0,2],[1,1],[1,2],[2,2]]
--
repCombination :: [a] -> Int -> [[a]]
repCombination ns n = comb n [(ns, [])]
    where
      comb 0 xs = [a | (_, a) <- xs]
      comb c xs = comb (c - 1) $ concatMap f xs
      f (xs, ys) = [(as, ys ++ [a]) | as@(a : _) <- tails xs]



-----------------------------------------
-- リスト操作
-----------------------------------------

--
-- リストの先頭から n 個目ごとに f を適用
--
-- ex : step 3 (\x -> 0) [1 .. 10] => [0,2,3,0,5,6,0,8,9,0]
--
step :: Int -> (a -> a) -> [a] -> [a]
step _ _ [] = []
step n f xs = f a : as ++ step n f bs
    where (a : as, bs) = splitAt n xs

--
-- リストから特定の要素だけを取り除く
--
-- ex : remove 3 [1,2,3,1,2,3] => [1,2,1,2]
--
remove :: Eq a => a -> [a] -> [a]
remove x ys = [y | y <- ys, y /= x]

--
-- すべての要素が異なっているか？
--
isAllDifferent :: Eq a => [a] -> Bool
isAllDifferent [] = True
isAllDifferent (x : xs)
    | elem x xs = False
    | otherwise = isAllDifferent xs

--
-- リストから指定された場所の要素を取り除く
--
deleteAt :: Int -> [a] -> [a]
deleteAt i xs = as ++ bs where (as, _ : bs) = splitAt i xs

--
-- リストを長さ n ごとに区切る
--
splits :: Int -> [a] -> [[a]]
splits _ [] = []
splits n xs = as : splits n bs where (as, bs) = splitAt n xs


-----------------------------------------
-- Zeller の公式（の変形 ?）
-----------------------------------------

--
-- 日付 -> 曜日 (数字を返す)
--   0 : Sun, 1 : Mon, 2 : Tue ..
--
zeller :: Int -> Int -> Int -> Int
zeller y m d
    | m < 3 = zeller (y - 1) (m + 12) d
    | otherwise = rem (y + a - b + c + d + e) 7
    where
      [a, b, c] = map (div y) [4, 100, 400]
      e = div (13 * m + 8) 5

--
-- 日付 -> 曜日 (文字列)
--
zellerToStr :: Int -> Int -> Int -> String
zellerToStr y m d = ws !! (zeller y m d)
    where ws = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
