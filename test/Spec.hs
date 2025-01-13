import Test.Hspec
import KWIC (splitWords, removeStopWords, generateCircularShifts, sortShifts, formatOutput)

main :: IO ()
main = hspec $ do
    describe "splitWords" $ do
        it "should split 'The quick, brown fox!' into ['The', 'quick', 'brown', 'fox']" $ do
            splitWords "The quick, brown fox!" `shouldBe`
                ["The", "quick", "brown", "fox"]
        it "should split 'Hello, world!' into ['Hello', 'world']" $ do
            splitWords "Hello, world!" `shouldBe`
                ["Hello", "world"]
        it "should handle an empty string" $ do
            splitWords "" `shouldBe` []
        it "should handle a string with only punctuation" $ do
            splitWords "!!!" `shouldBe` []

    describe "removeStopWords" $ do
        it "should remove 'The' from ['The', 'quick', 'brown', 'fox']" $ do
            removeStopWords ["The", "quick", "brown", "fox"] `shouldBe`
                ["quick", "brown", "fox"]
        it "should remove 'a' and 'the' from ['a', 'cat', 'is', 'on', 'the', 'roof']" $ do
            removeStopWords ["a", "cat", "is", "on", "the", "roof"] `shouldBe` 
                ["cat", "roof"]
        it "should handle an empty list" $ do
            removeStopWords [] `shouldBe` []

    describe "generateCircularShifts" $ do
        it "should generate circular shifts for 'The quick brown fox'" $ do
            generateCircularShifts ["The quick brown fox"] `shouldBe`
                [
                ("quick brown fox The", "The quick brown fox"),
                ("brown fox The quick", "The quick brown fox"),
                ("fox The quick brown", "The quick brown fox")
                ]
        it "should generate circular shifts for a single word" $ do
            generateCircularShifts ["hello"] `shouldBe`
                [("hello", "hello")]
        it "should handle an empty list" $ do
            generateCircularShifts [] `shouldBe` []

    describe "sortShifts" $ do
        it "should sort the circular shifts correctly" $ do
            let shifts = [
                        ("brown fox The quick", "The quick brown fox"),
                        ("fox The quick brown", "The quick brown fox"),
                        ("quick brown fox The", "The quick brown fox")
                        ]
            sortShifts shifts `shouldBe`
                        [
                        ("brown fox The quick", "The quick brown fox"),
                        ("fox The quick brown", "The quick brown fox"),
                        ("quick brown fox The", "The quick brown fox")
                        ]
        it "should handle an already sorted list" $ do
            let shifts =    [
                            ("brown fox The quick", "The quick brown fox"),
                            ("fox The quick brown", "The quick brown fox")
                            ]
            sortShifts shifts `shouldBe`
                            [
                            ("brown fox The quick", "The quick brown fox"),
                            ("fox The quick brown", "The quick brown fox")
                            ]
        it "should handle an empty list" $ do
            sortShifts [] `shouldBe` []

    describe "formatOutput" $ do
        it "should format the circular shifts correctly" $ do
            let formattedShifts =   [
                                    ("brown fox The quick", "The quick brown fox"),
                                    ("quick brown fox The", "The quick brown fox")
                                    ]
            formatOutput formattedShifts `shouldBe`
                "brown fox The quick (from \"The quick brown fox\")\nquick brown fox The (from \"The quick brown fox\")\n"
        it "should handle an empty list of shifts" $ do
            formatOutput [] `shouldBe` ""
        it "should format a single shift correctly" $ do
            let formattedShifts = [("quick brown fox The", "The quick brown fox")]
            formatOutput formattedShifts `shouldBe`
                "quick brown fox The (from \"The quick brown fox\")\n"
        it "should handle large inputs correctly" $ do
            let largeShifts = [(replicate 100 'a', "a"), (replicate 100 'b', "b")]
            formatOutput largeShifts `shouldBe`
                replicate 100 'a' ++ " (from \"a\")\n" ++ replicate 100 'b' ++ " (from \"b\")\n"
