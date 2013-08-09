import Test.Hspec (hspec, describe, it, shouldBe)

import Parse (parseLambdaUnsafe)
import Types

main = hspec $ do
  describe "Parse" $ do
    it "parses words into variables" $ do
      parseLambdaUnsafe "hello" `shouldBe` V "hello"

    it "parses (λ a a) as a lambda with parameter 'a' and body 'a'" $ do
      parseLambdaUnsafe "(λ a a)" `shouldBe` L (V "a") (V "a")

    it "parses parentheses starting with a variable as application" $ do
      parseLambdaUnsafe "(a b)" `shouldBe` A (V "a") (V "b")

    it "parses parthentheses starting with a lambda as an application" $ do
      parseLambdaUnsafe "((λ a a) b)" `shouldBe` A (L (V "a") (V "a")) (V "b")
