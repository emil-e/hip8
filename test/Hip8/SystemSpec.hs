module Hip8.SystemSpec (spec) where

import Hip8.System
import Hip8.Generators
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Vector.Generic as Vector
import Control.Applicative
import Control.Monad

isError :: System a -> Environment -> SystemState -> Bool
isError sys env state =
  case evalSystem env state sys of
    Left _  -> True
    Right _ -> False

isNonMutating :: System a -> Environment -> SystemState -> Bool
isNonMutating sys env state = Right state == execSystem env state sys

spec :: Spec
spec = do
  describe "getMem" $ do
    prop "returns what was written by setMem" $
      forAll writableAddress $ \addr x env state ->
        evalSystem env state (setMem addr x >> getMem addr) == Right x

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
      forAll validRegisterIndex $ \index x env state ->
        evalSystem env state (setReg index x >> getReg index) == Right x

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
      \x env state -> evalSystem env state (setRegI x >> getRegI) == Right x

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
      forAll validPC $ \addr env state ->
        evalSystem env state (setPC addr >> getPC) == Right addr

    prop "only changes the PC" $
      forAll validPC $ \addr ->
        isNonMutating $ do
          old <- getPC
          setPC addr
          setPC old

  describe "pop" $ do
    prop "pop fails on empty stack" $
      \env -> isError pop env initialSystemState
              
    prop "pop returns elements in the reverse order they were pushed" $
      \elems env state ->
        let popped = evalSystem env state $ do
              mapM_ push elems
              replicateM (length elems) pop
        in popped == Right (reverse elems)
      
