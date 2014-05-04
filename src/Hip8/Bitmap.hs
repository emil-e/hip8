{-|
Module     : Hip8.Bitmap
Maintainer : Emil Eriksson <shadewind@gmail.com>

Functions for working with bitmaps.
-}

module Hip8.Bitmap (
  Bitmap,
  dimensions,
  buffer,
  make,
  black,
  pixelAt,
  setPixelAt,
  toString,
  blit
  ) where

import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector
import Data.Word
import Data.Bits

-- |A bitmap with a given width and height.
data Bitmap = Bitmap (Int, Int) (Vector Word8)
                   deriving (Eq, Show)

-- |The width and height of the bitmap.
dimensions :: Bitmap -> (Int, Int)
dimensions (Bitmap dim _) = dim

-- |The bitmap. Data is stored in packed bytes.
buffer :: Bitmap -> Vector Word8
buffer (Bitmap _ buf) = buf

-- |Creates a new 'Bitmap' with the given data. The data should be given in row major format
-- with each bit representing a pixel. The width must be a multiple of 8.
make :: (Int, Int) -> Vector Word8 -> Bitmap
make (width, height) vec
  | (width `rem` 8) /= 0 = error "Width must be divisible by 8"
  | width <= 0 = error "Width must be >0"
  | height <= 0 = error "Height must be >0"
  | Vector.length vec /= n = error $ "Wrong number of elements in vector, should be " ++ show n
  | otherwise = Bitmap (width, height) vec
  where n = (width `quot` 8) * height

-- |Creates an black bitmap of the given size.
black :: (Int, Int) -> Bitmap
black (width, height) = make (width, height) $ Vector.replicate n 0
  where n = (width `quot` 8) * height

-- |Tests whether the pixel at the given coordinates is set.
pixelAt :: Bitmap -> (Int, Int) -> Bool
pixelAt disp@(Bitmap _ buf) c@(x, _) =
  testBit (buf ! byteOffset disp c) (7 - (x `rem` 8))

-- |Returns a new 'Bitmap' with the pixel at the given coordinates set to the
-- specified value.
setPixelAt :: Bitmap -> (Int, Int) -> Bool -> Bitmap
setPixelAt disp@(Bitmap dim buf) c@(x, _) set =
  Bitmap dim $ Vector.modify (\mbuf -> MVector.write mbuf offset byte) buf
  where byte = (if set then setBit else clearBit) (buf ! offset) (7 - (x `rem` 8))
        offset = byteOffset disp c

-- |Returns the byte offset for the given coordinates into the given bitmap.
byteOffset :: Bitmap -> (Int, Int) -> Int
byteOffset (Bitmap (width, height) _) (x, y) =
  (y `rem` height) * (width `quot` 8) + ((x `rem` width) `quot` 8)

-- |Returns a printable string representation of the given bitmap.
toString :: Bitmap -> String
toString bitmap@(Bitmap (width, height) _) =
  unlines (map rowWithIndex [0..(height - 1)])
  where rowWithIndex row = map (charAt row) [0..(width - 1)]
        charAt row col = charFor $ pixelAt bitmap (col, row)
        charFor True = '#'
        charFor False = '.'

-- |Blits the source bitmap into the target bitmap by XORing each pixel. Returns a tuple
-- containing the resulting bitmap as well as a flag telling whether any pixels collided or not.
-- The source bitmap must be smaller than the target bitmap.
blit :: Bitmap      -- ^The target bitmap
     -> Bitmap      -- ^The sprite
     -> (Int, Int)  -- ^The X Y coordinates
     -> Bitmap
blit dest@(Bitmap (dw, dh) dbuf) (Bitmap (sw, _) sbuf) (ox, oy)
  | (dw < sw) || (dh < dw) = error "Destination smaller than source"
  | otherwise = Bitmap (dw, dh) $ Vector.accum xor dbuf $ blitAssocs (Vector.indexed sbuf)
-- So we're creating an association list that maps fragments of the source bitmap to indexes
-- in the destination bitmap. We then accumulate those into the destination bitmap using XOR
-- as the merging function.
  where bitOffset = ox `rem` 8
        blitAssocs vec
          | Vector.null vec = []
          | otherwise = let (i, byte) = Vector.head vec
                            x = (i * 8) `rem` sw + ox
                            y = (i * 8) `quot` sw + oy
                            -- Every byte will be split into two fragments since the target
                            -- coordinates may not be a multiple of 8. This means part of the
                            -- byte will end up in the target byte and the rest will end up
                            -- in the byte to its right.
                            leftIndex = byteOffset dest (x, y)
                            rightIndex = byteOffset dest (x + 8, y)
                            leftPart = shiftR byte bitOffset
                            rightPart = shiftL byte (8 - bitOffset) 
                        in (leftIndex, leftPart) :
                           (rightIndex, rightPart) :
                           blitAssocs  (Vector.tail vec)
