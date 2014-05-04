{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module     : Hip8.Generators
Maintainer : Emil Eriksson <shadewind@gmail.com>

QuickCheck generators and 'Arbitrary' instances for Hip8 types.
-}

module Hip8.Generators (
  dataVector
  ) where

import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import Control.Applicative
import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Unboxed as UVector

-- |Generates an arbitrary vector of a given length.
dataVector :: (Vector.Vector v a, Arbitrary a) => Int -> Gen (v a)
dataVector n = Vector.fromList <$> vector n

instance (Arbitrary a, UVector.Unbox a) => Arbitrary (UVector.Vector a) where
  arbitrary = Vector.fromList <$> arbitrary
  shrink x = Vector.fromList <$> shrink (Vector.toList x)
