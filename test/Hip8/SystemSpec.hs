{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hip8.SystemSpec (
  anyKey,
  sprite,

  Address(..),
  readableAddress,
  invalidAddress,
  writableAddress,
  readOnlyAddress,
  readableArea,
  writableArea,
  readOnlyArea,
  invalidArea,

  PC(..),
  validPC,
  pcWithStepMargin,

  Reg(..),
  validRegisterIndex,
  invalidRegisterIndex,

  Hip8.SystemSpec.spec,
  isError,
  isNonMutating
  ) where

import Hip8.System
import Hip8.Generators
import Hip8.BitmapSpec
import Hip8.Bitmap (Bitmap)
import qualified Hip8.Bitmap as Bitmap
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Test.QuickCheck.Property as Prop
import qualified Data.Vector.Generic as Vector
import Control.Applicative
import Control.Monad
import Data.Word
import System.Random

instance Arbitrary Environment where
  arbitrary = Environment
              <$> (arbitrary `suchThat` (>0.0))
              <*> oneof [pure Nothing, Just <$> anyKey]

-- |Generates a valid key press
anyKey :: Gen Word8
anyKey = choose (0x0, 0xF)

-- |Generates a bitmap which is smaller than the display.
sprite :: Gen Bitmap
sprite = smallerBitmap initialDisplay

instance Arbitrary SystemState where
  arbitrary = do
    mem <- dataVector $ fromIntegral (memorySize - userMemoryStart)
    reg <- vector $ fromIntegral numRegisters
    regi <- readableAddress
    stk <- listOf $ pcWithStepMargin 1
    pc <- pcWithStepMargin 1
    env <- arbitrary
    disp <- bitmapWithSize displaySize
    rnds <- randoms <$> mkStdGen <$> arbitrary

    let (Right state) = execSystem env initialSystemState $ do
          writeMem userMemoryStart mem
          forM_ (zip [0..] reg) $ uncurry setReg
          setRegI regi
          forM_ stk push
          setPC pc
          setRandoms rnds
          blit disp (0, 0)
    return state

-- Make 'Either' testable
instance (Testable prop, Show a) => Testable (Either a prop) where
  property (Left x) = property Prop.result { Prop.ok = Just False,
                                             Prop.reason = show x }
  property (Right x) = property x
  exhaustive _ = True

-- 'System' is tested through arbitrary states and environments.
instance (Testable prop) => Testable (System prop) where
  property action = property $ \env state -> evalSystem env state action

-- |Newtype for generating valid addresses.
newtype Address = Address { getAddress :: Word16 }
                deriving (Eq, Show, Ord, Num)

instance Arbitrary Address where
  arbitrary = Address <$> readableAddress

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

-- |Newtype for generating valid program counters with a step margin of 1.
newtype PC = PC { getProgramCounter :: Word16 }
           deriving (Eq, Show, Ord, Num)

instance Arbitrary PC where
  arbitrary = PC <$> pcWithStepMargin 1

-- |Generates a valid program counter.
validPC :: Gen Word16
validPC = readableAddress `suchThat` even

-- |Generates a program counter such that N steps can be taken without error (i.e. without yielding a
-- PC outside the memory bounds).
pcWithStepMargin :: Word16 -> Gen Word16
pcWithStepMargin n = validPC `suchThat` (< memorySize - fromIntegral n * 2)

-- |Newtype for generating valid register indexes.
newtype Reg = Reg { getRegisterIndex :: Word8 }
            deriving (Eq, Show, Ord, Num)

instance Arbitrary Reg where
  arbitrary = Reg <$> validRegisterIndex

-- |Generates a valid register index.
validRegisterIndex :: Gen Word8
validRegisterIndex = choose (0, numRegisters - 1)

-- |Generates an invalid register index.
invalidRegisterIndex :: Gen Word8
invalidRegisterIndex = arbitrary `suchThat` (>= numRegisters)

-- |Checks whether the given action produces an error when run with the given state
-- and 'Environment'.
isError :: System a -> Environment -> SystemState -> Bool
isError sys env state =
  case evalSystem env state sys of
    Left _  -> True
    Right _ -> False

-- |Checks whether the given action produces a state equal to the input state
-- when run with the given 'Environment' and 'SystemState'.
isNonMutating :: System a -> Environment -> SystemState -> Bool
isNonMutating sys env state = Right state == execSystem env state sys

spec :: Spec
spec = do
  describe "getMem" $ do
    prop "returns what was written by setMem" $
      forAll writableAddress $ \addr x ->
        do setMem addr x
           x' <- getMem addr
           return $ x' == x

    prop "fails for invalid memory addresses" $
      forAll invalidAddress $ \addr -> isError $ getMem addr

    prop "does not mutate the state" $
      forAll readableAddress $ \addr -> isNonMutating $ getMem addr

  describe "setMem" $ do
    prop "only changes the given address" $
      forAll writableAddress $ \addr x -> isNonMutating $ do
        old <- getMem addr
        setMem addr x
        setMem addr old

    prop "fails for invalid memoryAddresses" $
      forAll invalidAddress $ \addr x -> isError $ setMem addr x

    prop "fails for read-only memoryAddresses" $
      forAll readOnlyAddress $ \addr x -> isError $ setMem addr x

  describe "readMem" $ do
    prop "is equivalent to reading byte by byte" $
      forAll readableArea $ \(addr, len) env state ->
        let bulk = evalSystem env state (readMem addr len)
            oneByOne = evalSystem env state $
                         Vector.fromList <$> forM [addr..(addr + len - 1)] getMem
        in bulk == oneByOne

    prop "fails for invalid ranges" $
      forAll invalidArea $ \(addr, len) -> isError $ readMem addr len

    prop "read length is equal to requested length" $
      forAll readableArea $ \(addr, len) env state ->
        let (Right vec) = evalSystem env state (readMem addr len)
        in Vector.length vec == fromIntegral len

    prop "does not mutate the state" $
      forAll readableArea $ \(addr, len) -> isNonMutating (readMem addr len)

  describe "writeMem" $ do
    prop "is equivalient to writing byte by byte" $
      forAll writableArea $ \(addr, len) env state ->
      forAll (dataVector $ fromIntegral len) $ \vec ->
        let bulk = execSystem env state (writeMem addr vec)
            list = Vector.toList vec
            setN i = setMem (addr +  i)
            oneByOne = execSystem env state $ zipWithM_ setN [0..] list
        in bulk == oneByOne

    prop "leaves rest of state unchanged" $
      forAll writableArea $ \(addr, len) ->
      forAll (dataVector $ fromIntegral len) $ \vec ->
        isNonMutating $ do
          old <- readMem addr len
          writeMem addr vec
          writeMem addr old

    prop "empty write leaves state unchanged" $
      forAll writableAddress $ \addr ->
        isNonMutating $ writeMem addr Vector.empty

    let shouldFailForArea area =
          forAll area $ \(addr, len) ->
          forAll (dataVector $ fromIntegral len) $ \vec ->
            isError $ writeMem addr vec

    prop "fails for invalid ranges" $
      shouldFailForArea invalidArea

    prop "fails for read-only ranges" $
      shouldFailForArea readOnlyArea

  describe "getReg" $ do
    prop "fails for invalid register indexes" $
      forAll invalidRegisterIndex $ \index -> isError $ getReg index

    prop "does not mutate the state" $
      forAll validRegisterIndex $ \index -> isNonMutating (getReg index)

  describe "setReg" $ do
    prop "fails for invalid register indexes" $
      forAll invalidRegisterIndex $ \index x -> isError $ setReg index x

    prop "getReg returns what was written" $
      forAll validRegisterIndex $ \index x ->
        do setReg index x
           x' <- getReg index
           return $ x' == x

    prop "only changes the given index" $
      forAll validRegisterIndex $ \index x ->
        isNonMutating $ do
          old <- getReg index
          setReg index x
          setReg index old

  describe "getRegI" $
    prop "does not mutate the state" $
      isNonMutating getRegI

  describe "setRegI" $ do
    prop "getRegI returns what was set" $
      \x -> do setRegI x
               x' <- getRegI
               return $ x == x'

    prop "only changes the program I register" $
      \x -> isNonMutating $ do
        old <- getRegI
        setRegI x
        setRegI old

  describe "getPC" $
    prop "does not mutate the state" $
      isNonMutating getPC

  describe "setPC" $ do
    prop "fails for odd addresses" $
      forAll (readableAddress `suchThat` odd) $ \addr -> isError $ setPC addr

    prop "fails for invalid addresses" $
      forAll (invalidAddress `suchThat` even) $ \addr -> isError $ setPC addr

    prop "getPC returns what was set" $
      forAll validPC $ \addr -> do setPC addr
                                   x <- getPC
                                   return $ x == addr


    prop "only changes the PC" $
      forAll validPC $ \addr ->
        isNonMutating $ do
          old <- getPC
          setPC addr
          setPC old

  describe "stepPC" $ do
    prop "increases PC by 2" $
      do pc <- getPC
         stepPC
         pc' <- getPC
         return $ pc' == (pc + 2)

    prop "fails if at last instruction" $
      isError $ setPC (memorySize - 2) >> stepPC

  describe "pop" $ do
    prop "pop fails on empty stack" $
      \env -> isError pop env initialSystemState

    prop "pop returns elements in the reverse order they were pushed" $
      \elems -> do mapM_ push elems
                   popped <- replicateM (length elems) pop
                   return $ popped == reverse elems

  describe "blit" $
    prop "blits the given bitmap to the display" $
      forAll sprite $ \bitmap (Positive x, Positive y) ->
       do st <- getSystemState
          blit bitmap (x, y)
          st' <- getSystemState
          return $
            display st'
            ==
            Bitmap.blit (display st) bitmap (x, y)

  describe "clearDisplay" $
    prop "makes the screen black" $
      \env state -> (display <$> execSystem env state clearDisplay)
                    ==
                    Right initialDisplay

  describe "setRandoms/nextRandom" $ do
    prop "returns the randoms set" $
      \rnds -> do setRandoms rnds
                  result <- replicateM (length rnds) nextRandom
                  return $ result == rnds
