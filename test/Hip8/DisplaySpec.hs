module Hip8.DisplaySpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Hip8.Display
import Hip8.Generators ()

spec :: Spec
spec = do
  describe "pixelAt/setPixelAt" $ do
    prop "pixelAt return what setPixelAt set" $
      \(Positive x, Positive y) disp p ->
        pixelAt (setPixelAt disp (x, y) p) (x, y) == p
