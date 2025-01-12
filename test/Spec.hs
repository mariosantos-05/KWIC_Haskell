import Test.Hspec
import KWIC (generateKWIC)

main :: IO ()
main = hspec $ do
    describe "generateKWIC" $ do

        it "handles multiple titles" $ do
            let input = ["The quick brown fox", "jumps over the lazy dog"]
            let expected = [ ("brown", "brown fox The quick")
                            , ("fox", "fox The quick brown")
                            , ("quick", "quick brown fox The")
                            , ("dog", "dog jumps over the lazy")
                            , ("jumps", "jumps over the lazy dog")
                            , ("lazy", "lazy dog jumps over the")
                            , ("over", "over the lazy dog jumps")
                            ]
            generateKWIC input `shouldBe` expected

        it "ignores stop words in titles" $ do
            let input = ["The quick brown fox is jumping over the lazy dog"]
            let expected = [ ("brown", "brown fox is jumping over the lazy dog The quick")
                            , ("dog", "dog The quick brown fox is jumping over the lazy")
                            , ("fox", "fox is jumping over the lazy dog The quick brown")
                            , ("jumping", "jumping over the lazy dog The quick brown fox is")
                            , ("lazy", "lazy dog The quick brown fox is jumping over the")
                            , ("over", "over the lazy dog The quick brown fox is jumping")
                            , ("quick", "quick brown fox is jumping over the lazy dog The")
                            ]
            generateKWIC input `shouldBe` expected

        it "returns empty result for empty input" $ do
            generateKWIC [] `shouldBe` []

        it "returns empty result for title with only stop words" $ do
            let input = ["the is of a"]
            generateKWIC input `shouldBe` []

        it "returns correct result for title with only one word" $ do
            let input = ["jumping"]
            let expected = [("jumping", "jumping")]
            generateKWIC input `shouldBe` expected

        it "handles case insensitivity of stop words" $ do
            let input = ["A quick brown fox jumps over The lazy dog"]
            let expected = [ ("brown", "brown fox jumps over The lazy dog A quick")
                            , ("dog", "dog A quick brown fox jumps over The lazy")
                            , ("fox", "fox jumps over The lazy dog A quick brown")
                            , ("jumps", "jumps over The lazy dog A quick brown fox")
                            , ("lazy", "lazy dog A quick brown fox jumps over The")
                            , ("over", "over The lazy dog A quick brown fox jumps")
                            , ("quick", "quick brown fox jumps over The lazy dog A")
                            ]
            generateKWIC input `shouldBe` expected