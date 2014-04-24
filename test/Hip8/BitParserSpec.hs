module Hip8.BitParserSpec (spec) where

import Control.Applicative
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Bits
import Data.Word
import Data.Maybe
import Hip8.BitParser

zebra :: (Bits a, Num a) => a
zebra = makeZebra 0
  where makeZebra x | testBit x 0 = x
                    | otherwise = makeZebra $ setBit (shiftR x 2) (bitSize x - 2)

bitRange :: Gen Int
bitRange = choose (0, 32)

invalidBitRequest :: Gen (Int, Int)
invalidBitRequest = liftA2 (,) bitRange bitRange `suchThat` uncurry (>)

validBitRequest :: Gen (Int, Int)
validBitRequest = liftA2 (,) bitRange bitRange `suchThat` uncurry (<=)

spec :: Spec
spec = do
  describe "maskBits" $
    prop "leaves correct number of bits" $
      forAll bitRange $ \n ->
      popCount (maskBits n (0xFFFFFFFF :: Word32)) == n
        
  describe "peekBits" $ do
    prop "fails if there are not enough bits left" $
      forAll invalidBitRequest $ \(n, r) ->
      isNothing $ runParser r (0 :: Word32) $ peekBits n

    prop "reads the correct number of bits from a zebra pattern" $
      forAll bitRange $ \n -> let result = runParser 32 (zebra :: Word32) $ peekBits n
                              in (popCount <$> result) == Just (n `quot` 2)

  describe "skipBits" $ do
    prop "fails if there are not enough bits left" $
      forAll invalidBitRequest $ \(n, r) ->
      isNothing $ runParser r (0 :: Word32) $ skipBits n

    prop "skips the correct number of bits" $
      forAll validBitRequest $ \(n, r) ->
        runParser r (0 :: Word32) (skipBits n >> getBitsLeft) == Just (r - n)

  describe "getBits" $ do
    prop "fails if there are not enough bits left" $
      forAll invalidBitRequest $ \(n, r) ->
      isNothing $ runParser r (0 :: Word32) $ getBits n

    prop "skips the correct number of bits" $
      forAll validBitRequest $ \(n, r) ->
      runParser r (0 :: Word32) (getBits n >> getBitsLeft) == Just (r - n)

    prop "read successive bit sequences" $
      forAll bitRange $ \x ->
      let parser = do left <- getBits x
                      right <- getBits $ 32 - x
                      return $ shiftL left (32 - x) .|. right
      in runParser 32 zebra parser == Just (zebra :: Word32)

  describe "getBitsLeft" $
    prop "returns the number of bits left" $
      forAll bitRange $ \n -> runParser n (0 :: Word32) getBitsLeft == Just n

  describe "assertEqBits" $ do
    prop "fails if there are not enough bits left" $
      forAll invalidBitRequest $ \(n, r) ->
      isNothing $ runParser r (0 :: Word32) $ assertEqBits n 0
      
    prop "fails on inequality" $
      forAll (bitRange `suchThat` (>0)) $ \n a b ->
        (b /= shiftR a (32 - n)) ==> isNothing $ runParser 32 (a :: Word32) (assertEqBits n b)

    prop "succeds on equality" $
      forAll (bitRange `suchThat` even) $ \n ->
        runParser 32 (zebra :: Word32) (assertEqBits n (maskBits n zebra)) == Just ()
