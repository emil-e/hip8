{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hip8.BitmapSpec (
  anyBlackBitmap,
  bitmapWithSize,
  smallerDimensions,
  smallerBitmap,
  smallerBlackBitmap,
  xyWithin,
  spec
  ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Hip8.Bitmap
import Hip8.Generators
import Control.Applicative
import Data.Bits
import qualified Data.Vector.Generic as Vector

instance Arbitrary Bitmap where
  arbitrary = do
    width <- bitmapWidth
    height <- arbitrary `suchThat` (>0)
    bitmapWithSize (width, height)

bitmapWidth :: Gen Int
bitmapWidth = sized $ \size -> (8 *) <$> resize (size `quot` 8) (arbitrary `suchThat` (>0))

anyBlackBitmap :: Gen Bitmap
anyBlackBitmap = do
  width <- bitmapWidth
  height <- arbitrary `suchThat` (>0)
  return $ emptyBitmap (width, height)

bitmapWithSize :: (Int, Int) -> Gen Bitmap
bitmapWithSize (width, height) = do
  vec <- dataVector (height * (width `quot` 8))
  return $ makeBitmap (width, height) vec

smallerDimensions :: Bitmap -> Gen (Int, Int)
smallerDimensions bitmap = do
  let (w, h) = dimensions bitmap
  width <- bitmapWidth `suchThat` (<= w)
  height <- choose (1, h)
  return (width, height)

smallerBitmap :: Bitmap -> Gen Bitmap
smallerBitmap bitmap = smallerDimensions bitmap >>= bitmapWithSize

smallerBlackBitmap :: Bitmap -> Gen Bitmap
smallerBlackBitmap bitmap = emptyBitmap <$> smallerDimensions bitmap

xyWithin :: Bitmap -> Gen (Int, Int)
xyWithin bitmap = let (w, h) = dimensions bitmap
                  in liftA2 (,) (choose (0, w - 1)) (choose (0, h - 1))

spec :: Spec
spec = do
  describe "pixelAt" $
    prop "wraps around if outside bounds" $
      \bitmap (Positive x) (Positive y) (Positive nx) (Positive ny) ->
        let (width, height) = dimensions bitmap
        in pixelAt bitmap (x + nx * width, y + ny * height) == pixelAt bitmap (x, y)
  
  describe "setPixelAt" $ do
    prop "pixelAt returns what setPixelAt set" $
      \(Positive x) (Positive y) disp p ->
        pixelAt (setPixelAt disp (x, y) p) (x, y) == p

    prop "wraps around if outside bounds" $
      \(Positive x) (Positive y) (Positive nx) (Positive ny) ->
      forAll anyBlackBitmap $ \bitmap ->
        let (width, height) = dimensions bitmap
        in pixelAt (setPixelAt bitmap (x + nx * width, y + ny * height) True) (x, y)
            

  describe "blit" $ do
    prop "correct placement of single pixel" $
      \dest (Positive dx) (Positive dy) ->
      forAll (smallerBlackBitmap dest) $ \src' ->
      forAll (xyWithin src') $ \(x, y) ->
        let src = setPixelAt src' (x, y) True
            result = blit dest src (dx, dy)
            c = (x + dx, y + dy)
        in pixelAt result c /= pixelAt dest c

    prop "wraps around if outside bounds" $
      \dest (Positive x) (Positive y) (Positive nx) (Positive ny) ->
      forAll (smallerBitmap dest) $ \src ->
        let (width, height) = dimensions dest
        in blit dest src (x, y) == blit dest src (x + nx * width, y + ny * height)

    prop "double blit yields original bitmap" $
      \dest (Positive x) (Positive y) ->
      forAll (smallerBitmap dest) $ \src ->
        blit (blit dest src (x, y)) src (x, y) == dest

    prop "no pixel left behind" $
      \(Positive x) (Positive y) ->
      forAll anyBlackBitmap $ \dest ->
      forAll (smallerBitmap dest) $ \src ->
      let whitePixelCount bmp = Vector.sum $ Vector.map popCount $ buffer bmp
      in whitePixelCount (blit dest src (x, y)) == whitePixelCount src

    prop "blit at zero to same size destination yields source" $
      \src -> blit (emptyBitmap $ dimensions src) src (0, 0) == src

    prop "empty source doesn't change destination" $
      \dest (Positive x) (Positive y) ->
      forAll (smallerBlackBitmap dest) $ \src ->
        blit dest src (x, y) == dest

    prop "leaves rest of destination intact" $
      \dest (Positive dx) (Positive dy) ->
      forAll (smallerBlackBitmap dest) $ \src' ->
      forAll (xyWithin src') $ \(x, y) ->
        let src = setPixelAt src' (x, y) True
            c = (x + dx, y + dy)
        in setPixelAt (blit dest src (dx, dy)) c (pixelAt dest c) == dest
