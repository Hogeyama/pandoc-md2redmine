module MyLibSpec (spec) where

import MyLib
import RIO
import Test.Hspec

spec :: Spec
spec = do
  describe "someFunc" $ do
    it "should return unit" $ do
      someFunc `shouldReturn` ()
