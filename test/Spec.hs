import Test.Hspec

spec :: Spec
spec = 
    describe "trivial test" $ do
        context "dont know" $
            it "55 should be 55" $
                55 `shouldBe` 55
    
main :: IO ()
main = hspec spec