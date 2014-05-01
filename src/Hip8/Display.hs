{-|
Module     : Hip8.Display
Maintainer : Emil Eriksson <shadewind@gmail.com>

Functions for managing the Hip8 display.
-}

module Hip8.Display (
  DisplayBuffer,
  dimensions,
  buffer,
  pixelAt,
  setPixelAt,
  emptyDisplay,
  blitDisplay
  ) where

import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector
import Data.Word
import Data.Bits
import Control.Monad.ST

-- |A display buffer with a given width and height.
data DisplayBuffer = DisplayBuffer (Int, Int) (Vector Word8)
                   deriving (Eq, Show)

-- |The width and height of the buffer
dimensions :: DisplayBuffer -> (Int, Int)
dimensions (DisplayBuffer dim _) = dim

-- |The data buffer. Data is stored in packed bytes.
buffer :: DisplayBuffer -> Vector Word8
buffer (DisplayBuffer _ buf) = buf

-- |Tests whether the pixel at the given coordinates is set.
pixelAt :: DisplayBuffer -> (Int, Int) -> Bool
pixelAt disp@(DisplayBuffer _ buf) c@(x, _) =
  testBit (buf ! byteOffset disp c) (x `rem` 8)

-- |Returns a new 'DisplayBuffer' with the pixel at the given coordinates set to the
-- specified value.
setPixelAt :: DisplayBuffer -> (Int, Int) -> Bool -> DisplayBuffer
setPixelAt disp@(DisplayBuffer dim buf) c@(x, _) set =
  DisplayBuffer dim $ Vector.modify (\mbuf -> MVector.write mbuf offset byte) buf
  where byte = (if set then setBit else clearBit) (buf ! offset) (x `rem` 8)
        offset = byteOffset disp c
          

-- |Creates an empty display of the given size.
emptyDisplay :: (Int, Int) -> DisplayBuffer
emptyDisplay (width, height)
  | (width `rem` 8) /= 0 = error "Width must be divisible by 8"
  | width <= 0 = error "Width must be >0"
  | height <= 0 = error "Height must be >0"
  | otherwise = DisplayBuffer (width, height) $ Vector.replicate n 0
  where n = (width `quot` 8) * height

-- |Returns the byte offset 
byteOffset :: DisplayBuffer -> (Int, Int) -> Int
byteOffset (DisplayBuffer (width, height) _) (x, y) =
  (y `rem` height) * (width `quot` 8) + ((x `rem` width) `quot` 8)

-- |Returns a new display buffer with the given sprite vector blitted into it.
-- A tuple of the new display and a flag indicating if any pixels collided is returned.
blitDisplay :: DisplayBuffer -- ^The display buffer
            -> Vector Word8  -- ^The sprite
            -> (Int, Int)    -- ^The X Y coordinates
            -> (DisplayBuffer, Bool)
blitDisplay disp@(DisplayBuffer dim buf) sprite (ox, oy) =
  let bitOffset = ox `rem` 8
  in runST $ do
    newBuf <- Vector.thaw buf
    collisions <- Vector.forM (Vector.indexed sprite) $ \(i, x) -> do
      -- The byte will be split between two target buffer bytes. Let's call them left
      -- and right.
      let offsetL = byteOffset disp (ox, oy + i)
          offsetR = byteOffset disp (ox + 8, oy + i)
          xl = shiftR x bitOffset
          xr = shiftL x (8 - bitOffset)

      xl' <- MVector.read newBuf offsetL
      xr' <- MVector.read newBuf offsetR
      MVector.write newBuf offsetL (xl' `xor` xl)
      MVector.write newBuf offsetR (xr' `xor` xr)
      return $ ((xl' .&. xl) .|. (xr' .&. xr)) /= 0

    frozenBuf <- Vector.unsafeFreeze newBuf
    return (DisplayBuffer dim frozenBuf, Vector.any id collisions)
