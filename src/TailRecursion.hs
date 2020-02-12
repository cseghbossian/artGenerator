

module TailRecursion where

import Prelude hiding (lookup)

--------------------------------------------------------------------------------

{- | `assoc def key [(k1,v1), (k2,v2), (k3,v3);...])`

     searches the list for the first i such that `ki` = `key`.
     If such a ki is found, then vi is returned.
     Otherwise, if no such ki exists in the list, `def` is returned.

     ** your function should be tail recursive **
 -}

-- >>> assoc 0 "william" [("ranjit", 85), ("william",23), ("moose",44)])
-- 23
--
-- >>> assoc 0 "bob" [("ranjit",85), ("william",23), ("moose",44)]
-- 0

assoc :: Int -> String -> [(String, Int)] -> Int
assoc def key [] = def
assoc def key ((k,v):xs) = if k == key then v else assoc def key xs 

--------------------------------------------------------------------------------
{- | `removeDuplicates l`

     returns the list of elements of `l` with duplicates
     that is, second, third ... occurrences, removed,
     and where the remaining elements appear in the
     same order as in l.

     ** your `helper` should be tail recursive **

     for this problem only, you may use the library functions:

      * elem
 -}

-- >>> removeDuplicates [1,6,2,4,12,2,13,12,6,9,13]
-- [1,6,2,4,12,13,9]

removeDuplicates :: [Int] -> [Int]
removeDuplicates l = reverse (helper [] l)
  where
    helper :: [Int] -> [Int] -> [Int]
    helper seen []     = seen
    helper seen (x:xs) = helper seen' rest'
      where
        seen'          = if (elem x seen) then seen else (x:seen)
        rest'          = xs

--------------------------------------------------------------------------------
{- | `wwhile f x` returns `x'` where there exist values

      `v_0`,...,`v_n` such that

      - `x` is equal to `v_0`
      - `x'` is equal to `v_n`
      - for each `i` between `0` and `n-2`, we have `f v_i` equals `(true, v_i+1)`
      - `f v_n-1` equals `(false, v_n)`.

    ** your function should be tail recursive **
 -}

-- >>> let f x = let xx = x * x * x in (xx < 100, xx) in wwhile f 2
-- 512

wwhile :: (a -> (Bool, a)) -> a -> a
wwhile f n = if (fst (f n) == False) then (snd (f n)) else wwhile f (snd (f n))

--------------------------------------------------------------------------------
{- | The **fixpoint** of a function `f` starting at `x`

`fixpoint f x` returns the FIRST element of the sequence x0, x1, x2, ...

        x0 = x
        x1 = f x0
        x2 = f x1
        x3 = f x2
        .
        .
        .

      such that xn = f x_{n-1}

      That is,

      `fixpoint f x` should compute `f x` and then

      * IF x == f x then the fixpoint is `x`
      * OTHERWISE, the it is the (recursively computed) fixpoint of `f x`.

 -}

{- | Fill in the implementation of `fixpointL f x0` which returns

     the list [x_0, x_1, x_2, x_3, ... , x_n]

     where

       * x = x_0

       * f x_0 = x_1, f x_1 = x_2, f x_2 = x_3, ... f x_n = x_{n+1}

       * xn = x_{n+1}
  -}

fixpointL :: (Int -> Int) -> Int -> [Int]
fixpointL f x = if (f x) == x then (x:[]) else (x:(fixpointL f (f x)))

-- You should see the following behavior at the prompt:

-- >>> fixpointL collatz 1
-- [1]
-- >>> fixpointL collatz 2
-- [2,1]
-- >>> fixpointL collatz 3
-- [3,10,5,16,8,4,2,1]
-- >>> fixpointL collatz 4
-- [4,2,1]
-- >>> fixpointL collatz 5
-- [5,16,8,4,2,1]

-- >>> fixpointL g 0
-- [0, 1000000, 540302, 857553, 654289, 793480,701369,763959,722102,750418,731403,744238,735604,741425,737506,740147,738369,739567,738760,739304,738937,739184,739018,739130,739054,739106,739071,739094,739079,739089,739082,739087,739083,739086,739084,739085]
-- this is because cos 0.739085 is approximately 0.739085

g :: Int -> Int
g x = truncate (1e6 * cos (1e-6 * fromIntegral x))

collatz :: Int -> Int
collatz 1     = 1
collatz n
  | even n    = n `div` 2
  | otherwise = 3 * n + 1

--------------------------------------------------------------------------------
{- | Now refactor your implementation of `fixpointL` so that it just returns
     the LAST element of the list, i.e. the `xn` that is equal to `f xn`
  -}

fixpointW :: (Int -> Int) -> Int -> Int
fixpointW f x = wwhile wwf x
 where
   wwf :: Int -> (Bool, Int)
   wwf x' = if (f x') == x' then (False, x') else (True, f (f x'))

-- >>> fixpointW collatz 1
-- 1
-- >>> fixpointW collatz 2
-- 1
-- >>> fixpointW collatz 3
-- 1
-- >>> fixpointW collatz 4
-- 1
-- >>> fixpointW collatz 5
-- 1
-- >>> fixpointW g 0
-- 739085
