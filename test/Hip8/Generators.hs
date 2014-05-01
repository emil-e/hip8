{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module     : Hip8.Generators
Maintainer : Emil Eriksson <shadewind@gmail.com>

QuickCheck generators and 'Arbitrary' instances for Hip8 types.
-}

module Hip8.Generators (
  dataVector,
  anyKey,
  readableAddress,
  invalidAddress,
  writableAddress,
  readOnlyAddress,
  readableArea,
  writableArea,
  readOnlyArea,
  invalidArea,
  validPC,
  validRegisterIndex,
  invalidRegisterIndex
  ) where

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Control.Applicative
import Hip8.System
import Hip8.Display
import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Unboxed as UVector
import Control.Monad
import Data.Word
import Data.Ix
import Data.List

-- |Generates an arbitrary vector of a given length.
dataVector :: (Vector.Vector v a, Arbitrary a) => Int -> Gen (v a)
dataVector n = Vector.fromList <$> vector n

-- |Generates a valid key press
anyKey :: Gen Word8
anyKey = choose (0x0, 0xF)

instance (Arbitrary a, UVector.Unbox a) => Arbitrary (UVector.Vector a) where
  arbitrary = Vector.fromList <$> arbitrary
  shrink x = Vector.fromList <$> shrink (Vector.toList x)

instance Arbitrary DisplayBuffer where
  arbitrary = sized $ \size -> do
    width <- (8 *) <$> resize (size `quot` 8) (arbitrary `suchThat` (>0))
    height <- arbitrary `suchThat` (>0)

    let coords = range ((0, 0), (width - 1, height - 1))
    assocs <- zip coords <$> sequence (repeat arbitrary)
    let doSet disp (c, x) = setPixelAt disp c x
    return $ foldl' doSet (emptyDisplay (width, height)) assocs

instance Arbitrary Environment where
  arbitrary = Environment
              <$> (arbitrary `suchThat` (>0.0))
              <*> oneof [pure Nothing, Just <$> anyKey]

instance Arbitrary SystemState where
  arbitrary = do
    mem <- dataVector $ fromIntegral (memorySize - userMemoryStart)
    reg <- vector $ fromIntegral numRegisters
    regi <- readableAddress
    stack <- listOf validPC
    pc <- validPC
    env <- arbitrary

    let (Right state) = execSystem env initialSystemState $ do
          writeMem userMemoryStart mem
          forM_ (zip [0..] reg) $ uncurry setReg
          setRegI regi
          forM_ stack push
          setPC pc
    return state

-- |Generates a valid readable (but not necessarily writable) memory address.
readableAddress :: Gen Word16
readableAddress = choose (0, memorySize - 1)

-- |Generates a memory address which is neither readable or writable.
invalidAddress :: Gen Word16
invalidAddress = arbitrary `suchThat` (>= memorySize)

-- |Generates a valid readable/writable memory address.
writableAddress :: Gen Word16
writableAddress = choose (userMemoryStart, memorySize - 1)

-- |Generates a read-only address.
readOnlyAddress :: Gen Word16
readOnlyAddress = choose (0, userMemoryStart - 1)

-- |Generates a tuple containing an address and length denoting a readable (but not necessarily writable)
-- memory area.
readableArea :: Gen (Word16, Word16)
readableArea = do
  start <- readableAddress
  end <- choose (start, memorySize)
  return (start, end - start)

-- |Generates a tuple containing an address and length denoting a readable/writable memory area.
writableArea :: Gen (Word16, Word16)
writableArea = do
  start <- writableAddress
  end <- choose (start, memorySize)
  return (start, end - start)

-- |Generates a tuple containing an address and length denoting a memory area overlapping a read only
-- region.
readOnlyArea :: Gen (Word16, Word16)
readOnlyArea = do
  start <- readOnlyAddress
  end <- choose (start, memorySize)
  return (start, end - start)

-- |Generates a tuple containing an address and length denoting an invalid memory area.
invalidArea :: Gen (Word16, Word16)
invalidArea = oneof [invalidStart, invalidEnd]
  where invalidStart = do
          start <- invalidAddress
          end <- arbitrary `suchThat` (>= start)
          return (start, end - start)
        invalidEnd = do
          start <- readableAddress
          end <- invalidAddress
          return (start, end - start)

-- |Generates a valid program counter.
validPC :: Gen Word16
validPC = readableAddress `suchThat` even

-- |Generates a valid register index.
validRegisterIndex :: Gen Word8
validRegisterIndex = choose (0, numRegisters - 1)

-- |Generates an invalid register index.
invalidRegisterIndex :: Gen Word8
invalidRegisterIndex = arbitrary `suchThat` (>= numRegisters)
