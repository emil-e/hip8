module Hip8.BitParserSpec (spec) where

import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Bits
import Data.Word
import Hip8.BitParser

spec :: Spec
spec = do
  describe "maskBits" $ do
    prop "leaves correct number of bits" $
      forAll (choose (0, 32)) $ \n ->
        popCount (maskBits n (0xFFFFFFFF :: Word32)) == n
        
  describe "peekBits" $ do
    prop "fails if there are not enough bits left" $
      \nbits remBits -> (nbits > remBits) ==>
        runParser (peekBits nbits) remBits (0 :: Word32) == Nothing
