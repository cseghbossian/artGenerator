-- | Based on code by Chris Stone

{-# LANGUAGE FlexibleContexts #-}

module RandomArt where

import           Text.Printf (printf)
import           Control.Exception.Base (assert)
import           System.Random
import           System.IO.Unsafe
import           Control.Monad (forM_)
import           Control.Monad.ST
import           Codec.Picture.Png
import           Codec.Picture.Types
import qualified Data.Vector.Storable as V

--------------------------------------------------------------------------------
-- | A Data Type for Grayscale Expressions -------------------------------------
--------------------------------------------------------------------------------
data Expr
  = VarX
  | VarY
  | Sine      Expr
  | Cosine    Expr
  | Average   Expr Expr
  | Times     Expr Expr
  | Thresh    Expr Expr Expr Expr
  | Offset    Expr
  | AvgThree  Expr Expr Expr
  deriving (Show)

--------------------------------------------------------------------------------
-- | Some sample Expressions ---------------------------------------------------
--------------------------------------------------------------------------------

sampleExpr0 :: Expr
sampleExpr0 = Sine (Average VarX VarY)

sampleExpr1 :: Expr
sampleExpr1 = Thresh VarX VarY
                VarX
                (Times (Sine VarX) (Cosine (Average VarX VarY)))

sampleExpr2 :: Expr
sampleExpr2 = Thresh VarX VarY (Sine VarX) (Cosine VarY)

sampleExpr3 :: Expr
sampleExpr3 =
  Cosine
    (Sine
      (Times
        (Cosine
          (Average
            (Cosine VarX)
            (Times
              (Cosine
                (Cosine
                  (Average
                    (Times VarY VarY)
                    (Cosine VarX ))))
              (Cosine
                (Times
                  (Sine (Cosine VarY))
                  (Average (Sine VarX) (Times VarX VarX)))))))
        VarY))

--------------------------------------------------------------------------------
-- | Printing Expressions as Strings -------------------------------------------
--------------------------------------------------------------------------------

-- | `exprToString e` converts an Expr `e` into a `String` representation.
-- >>> exprToString sampleExpr0
-- "sin(pi*((x+y)/2))"
--
-- >>> exprToString sampleExpr1
-- "(x<y?x:sin(pi*x)*cos(pi*((x+y)/2)))"
--
-- >>> exprToString sampleExpr2
-- "(x<y?sin(pi*x):cos(pi*y))"

exprToString :: Expr -> String
exprToString VarX                 = "x"
exprToString VarY                 = "y"
exprToString (Sine e)             = "sin(pi*" ++ exprToString e ++ ")"
exprToString (Cosine e)           = "cos(pi*" ++ exprToString e ++ ")"
exprToString (Average e1 e2)      = "((" ++ exprToString e1 ++ "+" ++ exprToString e2 ++ ")/2)"
exprToString (Times e1 e2)        = exprToString e1 ++ "*" ++ exprToString e2
exprToString (Thresh e1 e2 e3 e4) = "(" ++ exprToString e1 ++ "<" ++ exprToString e2 ++ "?" ++ exprToString e3 ++ ":" ++ exprToString e4 ++ ")"
exprToString (Offset e)           = "(1-|" ++ exprToString e ++ "|)"
exprToString (AvgThree e1 e2 e3)  = "((" ++ exprToString e1 ++ "+" ++ exprToString e2 ++ "+" ++ exprToString e3 ++ ")/3)"

--------------------------------------------------------------------------------
-- | Evaluating Expressions at a given X, Y co-ordinate ------------------------
--------------------------------------------------------------------------------

-- >>> eval  0.5 (-0.5) sampleExpr0
-- 0.0
--
-- >>> eval  0.3 0.3    sampleExpr0
-- 0.8090169943749475
--
-- >>> eval  0.5 0.2    sampleExpr2
-- 0.8090169943749475

eval :: Double -> Double -> Expr -> Double
eval x y VarX                  = x
eval x y VarY                  = y
eval x y (Sine e)              = sin (pi * (eval x y e))
eval x y (Cosine e)            = cos (pi * (eval x y e))
eval x y (Average e1 e2)       = ((eval x y e1) + (eval x y e2)) / 2
eval x y (Times e1 e2) 	       = (eval x y e1) * (eval x y e2)
eval x y (Thresh e1 e2 e3 e4)  = if ((eval x y e1) < (eval x y e2)) then (eval x y e3) else (eval x y e4)
eval x y (Offset e)            = 1 - abs (eval x y e)
eval x y (AvgThree e1 e2 e3)   = ((eval x y e1) + (eval x y e2) + (eval x y e3)) / 2

evalFn :: Double -> Double -> Expr -> Double
evalFn x y e = assert (-1.0 <= rv && rv <= 1.0) rv
  where
    rv       = eval x y e

--------------------------------------------------------------------------------
-- | Building Expressions ------------------------------------------------------
--------------------------------------------------------------------------------
--
-- >>> buildS 0
-- VarX
--
-- >>> buildS 1
-- VarY
--
-- >>> buildS 2
-- Sine (Average VarY VarX)

buildS :: Int -> Expr
buildS 0 = VarX
buildS 1 = VarY
buildS n = Sine (Average (buildS (n-1)) (buildS (n-2)))


--------------------------------------------------------------------------------
-- | Building Random Expressions -----------------------------------------------
--------------------------------------------------------------------------------
--  `build d` returns an Expr of depth `d`.
--  A call to `rand n` will return a random number between (0..n-1)
--  change and extend the below to produce more interesting expressions

build :: Int -> Expr
build 0
  | r < 5     = VarX
  | otherwise = VarY
  where
    r         = rand 10
build d
  | r == 0    = Sine (build (d-1)) 
  | r == 1    = Cosine (build (d-1))
  | r == 2    = Cosine (Average (build (d-1)) (build (d-1)))
  | r == 3    = Sine (Average (build (d-1)) (build (d-1)))
  | r == 4    = Average (build (d-1)) (Sine (build (d-1)))
  | r == 5    = Thresh (Sine VarX) (Times VarX VarY) (build (d-1)) (build (d-1))
  | r == 6    = Offset (AvgThree (build (d-1)) VarX VarY)
  where
    r         = rand 7


--------------------------------------------------------------------------------
-- | Best Image "Seeds" --------------------------------------------------------
--------------------------------------------------------------------------------

-- grayscale
g1, g2, g3 :: (Int, Int)
g1 = (4,53)
g2 = (6,15)
g3 = (7,66)


-- color
c1, c2, c3 :: (Int, Int)
c1 = (4,28)
c2 = (3,13)
c3 = (10, 10)

--------------------------------------------------------------------------------
-- | Generating GrayScale Images -----------------------------------------------
--------------------------------------------------------------------------------
-- >>> emitRandomGray 150 (3, 12)
emitRandomGray :: Int -> (Int, Int) -> IO ()
emitRandomGray size (depth, seed) = do
  setStdGen (mkStdGen seed)
  let e    = build depth
  let file = printf "gray_%d_%d_%d.png" size depth seed
  emitGray file size e

-- >>> emitGray "sample.png" 150 sampleExpr3
emitGray :: FilePath -> Int -> Expr -> IO ()
emitGray file size e = writePng ("img/" ++ file) (imageGray size e)

-- | `imageGray n e`
--   converts an Expr `e`
--   into an `n * n` grayscale image
imageGray :: Int -> Expr -> Image Pixel8
imageGray n e = mkImg n (pixel8 n e)

--------------------------------------------------------------------------------
-- | Generating Color Images ---------------------------------------------------
--------------------------------------------------------------------------------
-- >>> emitRandomColor 150 (3, 12)
emitRandomColor :: Int -> (Int, Int) -> IO ()
emitRandomColor size (depth, seed) = do
  setStdGen (mkStdGen seed)
  let eR   = build depth
  let eG   = build depth
  let eB   = build depth
  let file = printf "color_%d_%d_%d.png" size depth seed
  emitColor file size eR eG eB

-- >>> emitColor "sample.png" 150 sampleExpr sampleExpr sampleExpr
emitColor :: FilePath -> Int -> Expr -> Expr -> Expr -> IO ()
emitColor file size eR eG eB = writeImg file (imageColor size eR eG eB)

-- | `imageColor n eR eG eB`
--   converts Exprs for Red, Green, Blue
--   into an `n * n` color image
imageColor :: Int -> Expr -> Expr -> Expr -> Image PixelRGB8
imageColor n eR eG eB = mkImg n (pixelRGB8 n eR eG eB)

--------------------------------------------------------------------------------
-- | Low level functions for creating pixels and images
--------------------------------------------------------------------------------
writeImg :: (PngSavable a) => FilePath -> Image a -> IO ()
writeImg file = writePng ("img/" ++ file)

-- mkImage :: V.Storable (PixelBaseComponent a) => Int -> (Int -> Int -> PixelBaseComponent a) -> Image a
mkImage :: (Pixel a) => Int -> (Int -> Int -> PixelBaseComponent a) -> Image a
mkImage n f = Image dim dim $ V.fromList pixels
  where
    dim     = 2 * n + 1
    pixels  = [ f x y | y <- [0 .. (dim - 1)]
                      , x <- [0 .. (dim - 1)]
              ]
mkImg :: (Pixel a) => Int -> (Int -> Int -> a) -> Image a
mkImg n f = runST $ do
  let dim = 2 * n + 1
  img <- newMutableImage dim dim
  forM_ [0 .. (dim - 1)] $ \x ->
    forM_ [0 .. (dim - 1)] $ \y ->
      writePixel img y x (f x y)
  unsafeFreezeImage img

pixelRGB8 :: Int -> Expr -> Expr -> Expr -> Int -> Int -> PixelRGB8
pixelRGB8 n eR eG eB x y = PixelRGB8 (pixel8 n eR x y)
                                     (pixel8 n eG x y)
                                     (pixel8 n eB x y)

pixel8 :: Int -> Expr -> Int -> Int -> Pixel8
pixel8 n e x y = toIntensity (evalFn (toReal n x) (toReal n y) e)

-- | `toReal n pos` converts pos in {0 .. 2n + 1} to [-1.0, 1.0]
toReal :: Int -> Int -> Double
toReal n pos = fromIntegral (pos - n) / fromIntegral n

-- | `toIntensity z` converts z in [-1.0, 1.0] to a [0, 255]
toIntensity :: Double -> Pixel8
toIntensity z = fromInteger (floor (127.5 + 127.5 * z))

--------------------------------------------------------------------------------
-- | `rand n` returns a random number between `0` and `n-1`
--------------------------------------------------------------------------------
rand :: Int -> Int
--------------------------------------------------------------------------------
rand n = unsafePerformIO $ do
  v <- randomIO
  return (v `mod` n)
