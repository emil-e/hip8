{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hip8.BitmapSpec (
  bitmapDimensions,
  bitmapWidth,
  blackBitmap,
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
import Hip8.Bitmap (Bitmap)
import qualified Hip8.Bitmap as Bitmap
import Hip8.Generators
import Control.Applicative

instance Arbitrary Bitmap where
  arbitrary = do
    width <- bitmapWidth
    height <- arbitrary `suchThat` (>0)
    bitmapWithSize (width, height)

bitmapDimensions :: Gen (Int, Int)
bitmapDimensions = do
  width <- bitmapWidth
  height <- arbitrary `suchThat` (>0)
  return (width, height)

bitmapWidth :: Gen Int
bitmapWidth = sized $ \size -> (8 *) <$> resize (size `quot` 8) (arbitrary `suchThat` (>0))

blackBitmap :: Gen Bitmap
blackBitmap = Bitmap.black <$> bitmapDimensions

whiteBitmap :: Gen Bitmap
whiteBitmap = Bitmap.white <$> bitmapDimensions

bitmapWithSize :: (Int, Int) -> Gen Bitmap
bitmapWithSize (width, height) = do
  vec <- dataVector (height * (width `quot` 8))
  return $ Bitmap.make (width, height) vec

smallerDimensions :: Bitmap -> Gen (Int, Int)
smallerDimensions bitmap = do
  let (w, h) = Bitmap.dimensions bitmap
  width <- bitmapWidth `suchThat` (<= w)
  height <- choose (1, h)
  return (width, height)

smallerBitmap :: Bitmap -> Gen Bitmap
smallerBitmap bitmap = smallerDimensions bitmap >>= bitmapWithSize

smallerBlackBitmap :: Bitmap -> Gen Bitmap
smallerBlackBitmap bitmap = Bitmap.black <$> smallerDimensions bitmap

xyWithin :: Bitmap -> Gen (Int, Int)
xyWithin bitmap = let (w, h) = Bitmap.dimensions bitmap
                  in liftA2 (,) (choose (0, w - 1)) (choose (0, h - 1))

xyOfColourWithin :: Bitmap -> Bool -> Gen (Int, Int)
xyOfColourWithin bmp v = xyWithin bmp `suchThat` (\c -> Bitmap.pixelAt bmp c == v)

spec :: Spec
spec = do
  describe "pixelAt" $
    prop "wraps around if outside bounds" $
      \bitmap (Positive x) (Positive y) (Positive nx) (Positive ny) ->
        let (width, height) = Bitmap.dimensions bitmap
        in Bitmap.pixelAt bitmap (x + nx * width, y + ny * height) == Bitmap.pixelAt bitmap (x, y)
  
  describe "setPixelAt" $ do
    prop "Bitmap.pixelAt returns what Bitmap.setPixelAt set" $
      \(Positive x) (Positive y) disp p ->
        Bitmap.pixelAt (Bitmap.setPixelAt disp (x, y) p) (x, y) == p

    prop "wraps around if outside bounds" $
      \(Positive x) (Positive y) (Positive nx) (Positive ny) ->
      forAll blackBitmap $ \bitmap ->
        let (width, height) = Bitmap.dimensions bitmap
        in Bitmap.pixelAt (Bitmap.setPixelAt bitmap (x + nx * width, y + ny * height) True) (x, y)
            

  describe "blit" $ do
    prop "correct placement of single pixel" $
      \dest (Positive dx) (Positive dy) ->
      forAll (smallerBlackBitmap dest) $ \src' ->
      forAll (xyWithin src') $ \(x, y) ->
        let src = Bitmap.setPixelAt src' (x, y) True
            result = Bitmap.blit dest src (dx, dy)
            c = (x + dx, y + dy)
        in Bitmap.pixelAt result c /= Bitmap.pixelAt dest c

    prop "wraps around if outside bounds" $
      \dest (Positive x) (Positive y) (Positive nx) (Positive ny) ->
      forAll (smallerBitmap dest) $ \src ->
        let (width, height) = Bitmap.dimensions dest
        in Bitmap.blit dest src (x, y) == Bitmap.blit dest src (x + nx * width, y + ny * height)

    prop "double Bitmap.blit yields original bitmap" $
      \dest (Positive x) (Positive y) ->
      forAll (smallerBitmap dest) $ \src ->
        Bitmap.blit (Bitmap.blit dest src (x, y)) src (x, y) == dest

    prop "no pixel left behind" $
      \(Positive x) (Positive y) ->
      forAll blackBitmap $ \dest ->
      forAll (smallerBitmap dest) $ \src ->
        Bitmap.numWhitePixels (Bitmap.blit dest src (x, y)) == Bitmap.numWhitePixels src

    prop "Bitmap.blit at zero to same size destination yields source" $
      \src -> Bitmap.blit (Bitmap.black $ Bitmap.dimensions src) src (0, 0) == src

    prop "empty source doesn't change destination" $
      \dest (Positive x) (Positive y) ->
      forAll (smallerBlackBitmap dest) $ \src ->
        Bitmap.blit dest src (x, y) == dest

    prop "leaves rest of destination intact" $
      \dest (Positive dx) (Positive dy) ->
      forAll (smallerBlackBitmap dest) $ \src' ->
      forAll (xyWithin src') $ \(x, y) ->
        let src = Bitmap.setPixelAt src' (x, y) True
            c = (x + dx, y + dy)
        in Bitmap.setPixelAt (Bitmap.blit dest src (dx, dy)) c (Bitmap.pixelAt dest c) == dest

  describe "numWhitePixels" $ do
    prop "is zero for black bitmaps" $
      forAll blackBitmap $ \bmp -> Bitmap.numWhitePixels bmp == 0

    prop "flipping a black pixel to white increases the count by one" $
      forAll (arbitrary `suchThat` (not . Bitmap.isWhite)) $ \bmp ->
      forAll (xyOfColourWithin bmp False) $ \c ->
        Bitmap.numWhitePixels (Bitmap.setPixelAt bmp c True) == Bitmap.numWhitePixels bmp + 1

    prop "flipping a white pixel to black decreases the count by one" $
      forAll (arbitrary `suchThat` (not . Bitmap.isBlack)) $ \bmp ->
      forAll (xyOfColourWithin bmp True) $ \c ->
        Bitmap.numWhitePixels (Bitmap.setPixelAt bmp c False) == Bitmap.numWhitePixels bmp - 1

  describe "black" $
    prop "isBlack returns true for all sizes" $
      forAll bitmapDimensions $ \dim -> Bitmap.isBlack $ Bitmap.black dim

  describe "white" $
    prop "isWhite returns true for all sizes" $
      forAll bitmapDimensions $ \dim -> Bitmap.isWhite $ Bitmap.white dim

  describe "isBlack" $
    prop "even a single pixel makes it return false" $
      forAll blackBitmap $ \bmp ->
      forAll (xyWithin bmp) $ \c ->
        not $ Bitmap.isBlack $ Bitmap.setPixelAt bmp c True

  describe "isWhite" $
    prop "even a single pixel makes it return false" $
      forAll whiteBitmap $ \bmp ->
      forAll (xyWithin bmp) $ \c ->
        not $ Bitmap.isWhite $ Bitmap.setPixelAt bmp c False
