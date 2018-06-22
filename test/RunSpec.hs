module RunSpec where

import Test.Hspec

spec :: Spec
spec = do
  it "fails" $ do
    True `shouldBe` False
