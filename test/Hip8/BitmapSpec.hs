{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hip8.BitmapSpec (
  bitmapDimensions,
  bitmapWidth,
  nonNullBitmap,
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
  arbitrary = bitmapDimensions >>= bitmapWithSize

-- |Generates a null bitmap.
nullBitmap :: Gen Bitmap
nullBitmap = do zeroWidth <- arbitrary
                if zeroWidth
                  then do
                    h <- getNonNegative <$> arbitrary
                    bitmapWithSize (0, h)
                  else do
                    w <- bitmapWidth
                    bitmapWithSize (w, 0)

-- |Generates a non-null bitmap.
nonNullBitmap :: Gen Bitmap
nonNullBitmap = arbitrary `suchThat` (not . Bitmap.isNull)

-- |Generates a set of valid bitmap dimensions.
bitmapDimensions :: Gen (Int, Int)
bitmapDimensions = do
  width <- bitmapWidth
  height <- getNonNegative <$> arbitrary
  return (width, height)

-- |Generates a valid bitmap width.
bitmapWidth :: Gen Int
bitmapWidth = sized $ \size -> do
  x <- resize (size `quot` 8) arbitrary
  return $ (getNonNegative x) * 8

-- |Generates an arbitrary black bitmap.
blackBitmap :: Gen Bitmap
blackBitmap = Bitmap.black <$> bitmapDimensions

-- |Generates an arbitrary white bitmap.
whiteBitmap :: Gen Bitmap
whiteBitmap = Bitmap.white <$> bitmapDimensions

-- |Generates an arbitrary bitmap of the given size.
bitmapWithSize :: (Int, Int) -> Gen Bitmap
bitmapWithSize (width, height) = do
  vec <- dataVector (height * (width `quot` 8))
  return $ Bitmap.make (width, height) vec

-- |Generates dimensions which are smaller or equal to the dimensions of the
-- given bitmap.
smallerDimensions :: Bitmap -> Gen (Int, Int)
smallerDimensions bitmap = do
  let (w, h) = Bitmap.dimensions bitmap
  width <- bitmapWidth `suchThat` (<= w)
  height <- choose (1, h)
  return (width, height)

-- |Generates an arbitrary bitmap which is smaller or the same size as the given bitmap.
smallerBitmap :: Bitmap -> Gen Bitmap
smallerBitmap bitmap = smallerDimensions bitmap >>= bitmapWithSize

-- |Generates a black bitmap which is smaller or the same size as the given bitmap.
smallerBlackBitmap :: Bitmap -> Gen Bitmap
smallerBlackBitmap bitmap = Bitmap.black <$> smallerDimensions bitmap

-- |Generates a pair of X/Y coordinates within the given bitmap.
xyWithin :: Bitmap -> Gen (Int, Int)
xyWithin bitmap = let (w, h) = Bitmap.dimensions bitmap
                  in liftA2 (,) (choose (0, w - 1)) (choose (0, h - 1))

-- |Generates a pair of X/Y coordinates denoting a pixel with the given color within the given bitmap.
xyOfColourWithin :: Bitmap -> Bool -> Gen (Int, Int)
xyOfColourWithin bmp v = xyWithin bmp `suchThat` (\c -> Bitmap.pixelAt bmp c == v)

spec :: Spec
spec = do
  describe "pixelAt" $
    prop "wraps around if outside bounds" $
      forAll nonNullBitmap $
        \bitmap (NonNegative x) (NonNegative y) (Positive nx) (Positive ny) ->
         let (width, height) = Bitmap.dimensions bitmap
         in Bitmap.pixelAt bitmap (x + nx * width, y + ny * height)
            ==
            Bitmap.pixelAt bitmap (x, y)

  describe "setPixelAt" $ do
    prop "Bitmap.pixelAt returns what Bitmap.setPixelAt set" $
      forAll nonNullBitmap $ \disp (Positive x) (Positive y) p ->
        Bitmap.pixelAt (Bitmap.setPixelAt disp (x, y) p) (x, y) == p

    prop "wraps around if outside bounds" $
      \(NonNegative x) (NonNegative y) (Positive nx) (Positive ny) ->
      forAll (blackBitmap `suchThat` (not . Bitmap.isNull)) $ \bitmap ->
        let (w, h) = Bitmap.dimensions bitmap
            bitmap' = Bitmap.setPixelAt bitmap (x + nx * w, y + ny * h) True
        in Bitmap.pixelAt bitmap' (x, y)


  describe "blit" $ do
    prop "correct placement of single pixel" $
      forAll nonNullBitmap $ \dest (Positive dx) (Positive dy) ->
      forAll (smallerBlackBitmap dest `suchThat` (not . Bitmap.isNull)) $
      \src' -> forAll (xyWithin src') $ \(x, y) ->
        let src = Bitmap.setPixelAt src' (x, y) True
            result = Bitmap.blit dest src (dx, dy)
            c = (x + dx, y + dy)
        in Bitmap.pixelAt result c /= Bitmap.pixelAt dest c

    prop "wraps around if outside bounds" $
      forAll nonNullBitmap $ \dest ->
      forAll (smallerBitmap dest) $ \src ->
      \(Positive x) (Positive y) (Positive nx) (Positive ny) ->
        let (w, h) = Bitmap.dimensions dest
        in Bitmap.blit dest src (x, y)
           ==
           Bitmap.blit dest src (x + nx * w, y + ny * h)

    prop "double Bitmap.blit yields original bitmap" $
      forAll nonNullBitmap $ \dest (Positive x) (Positive y) ->
      forAll (smallerBitmap dest) $ \src ->
        Bitmap.blit (Bitmap.blit dest src (x, y)) src (x, y) == dest

    prop "no pixel left behind" $
      \(Positive x) (Positive y) ->
      forAll (blackBitmap `suchThat` (not . Bitmap.isNull)) $ \dest ->
      forAll (smallerBitmap dest) $ \src ->
        Bitmap.numWhitePixels (Bitmap.blit dest src (x, y)) == Bitmap.numWhitePixels src

    prop "Bitmap.blit at zero to same size destination yields source" $
      \src -> Bitmap.blit (Bitmap.black $ Bitmap.dimensions src) src (0, 0) == src

    prop "empty source doesn't change destination" $
      forAll nonNullBitmap $ \dest (Positive x) (Positive y) ->
      forAll (smallerBlackBitmap dest) $ \src ->
        Bitmap.blit dest src (x, y) == dest

    prop "leaves rest of destination intact" $
      forAll nonNullBitmap $ \dest (Positive dx) (Positive dy) ->
      forAll (smallerBlackBitmap dest `suchThat` (not . Bitmap.isNull)) $
      \src -> forAll (xyWithin src) $ \(x, y) ->
        let src' = Bitmap.setPixelAt src (x, y) True
            c = (x + dx, y + dy)
            origPixel = Bitmap.pixelAt dest c
            dest' = Bitmap.blit dest src' (dx, dy)
        in Bitmap.setPixelAt dest' c origPixel == dest

  describe "numWhitePixels" $ do
    prop "is zero for black bitmaps" $
      forAll blackBitmap $ \bmp -> Bitmap.numWhitePixels bmp == 0

    prop "flipping a black pixel to white increases the count by one" $
      forAll (nonNullBitmap `suchThat` (not . Bitmap.isWhite)) $ \bmp ->
      forAll (xyOfColourWithin bmp False) $ \c ->
        Bitmap.numWhitePixels (Bitmap.setPixelAt bmp c True)
        ==
        Bitmap.numWhitePixels bmp + 1

    prop "flipping a white pixel to black decreases the count by one" $
      forAll (arbitrary `suchThat` (not . Bitmap.isBlack)) $ \bmp ->
      forAll (xyOfColourWithin bmp True) $ \c ->
        Bitmap.numWhitePixels (Bitmap.setPixelAt bmp c False)
        ==
        Bitmap.numWhitePixels bmp - 1

  describe "black" $
    prop "isBlack returns true for all sizes" $
      forAll bitmapDimensions $ \dim -> Bitmap.isBlack $ Bitmap.black dim

  describe "white" $
    prop "isWhite returns true for non-null sizes" $
      forAll (bitmapDimensions `suchThat` \(w, h) -> (w /= 0) && (h /= 0)) $
        \dim -> Bitmap.isWhite $ Bitmap.white dim

  describe "isBlack" $ do
    prop "even a single pixel makes it return false" $
      forAll (blackBitmap `suchThat` (not . Bitmap.isNull)) $ \bmp ->
      forAll (xyWithin bmp) $ \c ->
        not $ Bitmap.isBlack $ Bitmap.setPixelAt bmp c True

    prop "returns true for null bitmaps" $
      forAll nullBitmap Bitmap.isBlack

  describe "isWhite" $ do
    prop "even a single pixel makes it return false" $
      forAll (whiteBitmap `suchThat` (not . Bitmap.isNull)) $ \bmp ->
      forAll (xyWithin bmp) $ \c ->
        not $ Bitmap.isWhite $ Bitmap.setPixelAt bmp c False

    prop "returns false for null bitmaps" $
     forAll nullBitmap (not . Bitmap.isWhite)

  describe "isNull" $ do
    prop "returns True for null bitmaps" $
      forAll nullBitmap Bitmap.isNull

    prop "returns False for all other bitmaps" $
      forAll nonNullBitmap (not . Bitmap.isNull)
