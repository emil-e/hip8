module Hip8.SystemSpec (spec) where

import Hip8.System
import Hip8.Generators
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Vector.Generic as Vector
import Control.Applicative
import Control.Monad

isError :: Either a b -> Bool
isError (Left _) = True
isError (Right _) = False

isNonMutating :: System a -> Environment -> SystemState -> Bool
isNonMutating sys env state = Right state == execSystem env state sys

spec :: Spec
spec = do
  describe "getMem" $ do
    prop "returns what was written by setMem" $
      forAll writableAddress $ \addr x env state ->
        evalSystem env state (setMem addr x >> getMem addr) == Right x

    prop "fails for invalid memory addresses" $
      forAll invalidAddress $ \addr env state ->
        isError $ evalSystem env state (getMem addr)

    prop "does not mutate the state" $
      forAll readableAddress $ \addr -> isNonMutating (getMem addr)

  describe "setMem" $ do
    prop "only changes the given address" $
      forAll writableAddress $ \addr x -> isNonMutating $ do
        old <- getMem addr
        setMem addr x
        setMem addr old

    let shouldFailForAddress addrGen =
          forAll addrGen $ \addr x env state ->
            isError $ evalSystem env state (setMem addr x)

    prop "fails for invalid memoryAddresses" $
      shouldFailForAddress invalidAddress

    prop "fails for read-only memoryAddresses" $
      shouldFailForAddress readOnlyAddress

  describe "readMem" $ do
    prop "is equivalent to reading byte by byte" $
      forAll readableArea $ \(addr, len) env state ->
        let bulk = evalSystem env state (readMem addr len)
            oneByOne = evalSystem env state $
                         Vector.fromList <$> forM [addr..(addr + len - 1)] getMem
        in bulk == oneByOne

    prop "fails for invalid ranges" $
      forAll invalidArea $ \(addr, len) env state ->
        isError $ evalSystem env state (readMem addr len)

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
          forAll area $ \(addr, len) env state ->
          forAll (dataVector $ fromIntegral len) $ \vec ->
            isError $ evalSystem env state (writeMem addr vec)
    
    prop "fails for invalid ranges" $
      shouldFailForArea invalidArea

    prop "fails for invalid ranges" $
      shouldFailForArea readOnlyArea
