import Test.Hspec (hspec, describe, it, shouldBe)

import Parse       (parseLambdaUnsafe)
import PrettyPrint (prettyPrint)
import Eval        (alpha, beta, freeVariables)
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

  describe "Eval" $ do
    let p             = parseLambdaUnsafe
        reducesTo a b = prettyPrint (beta $ parseLambdaUnsafe a) `shouldBe` (prettyPrint . parseLambdaUnsafe) b

    describe "beta" $ do
      it "replaces the parameter of a lambda with the argument" $ do
        "((λ a a) b)"       `reducesTo` "b"
        "((λ a a) (λ b b))" `reducesTo` "(λ b b)"

      it "doesn't try to apply beta reduction to variables" $ do
        "(a (λ b b))" `reducesTo` "(a (λ b b))"

      it "replaces every occurrence of the parameter with the argument in both sides of an application" $ do
        "((λ a (a a)) b)" `reducesTo` "(b b)"

      it "replaces every ocurrence of the parameter in a lambda body" $ do
        "((λ a (λ b (a b))) c)" `reducesTo` "(λ b (c b))"

      it "does not replace bound variables in the body of a lambda" $ do
        "((λ b (λ b (a b))) c)" `reducesTo` "(λ b (a b))"

      it "avoids variable capture" $ do
        "((λ a (λ b a)) (λ x b))"                     `reducesTo` "(λ c (λ x b))"
        "((λ c (λ b c)) (λ x b))"                     `reducesTo` "(λ a (λ x b))"
        "((λ a (λ b (λ c a))) (λ x c))"               `reducesTo` "(λ b (λ b (λ x c)))"
        "((λ a (λ b (λ c a))) (λ x (b c)))"           `reducesTo` "(λ d (λ d (λ x (b c))))"
        "((λ a (λ b (λ c (λ d a)))) (λ x (d (b c))))" `reducesTo` "(λ e (λ e (λ e (λ x (d (b c))))))"
        "((λ a (λ b (λ c a))) (λ x (b c)))"           `reducesTo` "(λ d (λ d (λ x (b c))))"

    describe "freeVariables" $ do
      let hasFreeVariables a b = (map prettyPrint $ freeVariables $ parseLambdaUnsafe a) `shouldBe` (map prettyPrint b)

      it "returns a list of unbound variables in an expression" $ do
        "b"                 `hasFreeVariables` [V "b"]
        "(λ a b)"           `hasFreeVariables` [V "b"]
        "((λ a b) (λ c d))" `hasFreeVariables` [V "b", V "d"]

    describe "alpha" $ do
      let replacing   = id
          with a b    = alpha (parseLambdaUnsafe a) (parseLambdaUnsafe b)
          inside f e  = f (parseLambdaUnsafe e)
          becomes a b = (prettyPrint a) `shouldBe` (prettyPrint $ parseLambdaUnsafe b)

      it "changes the name of the given variable everywhere in the given expression" $ do
        replacing "a" `with` "b" `inside` "a"                 `becomes` "b"
        replacing "a" `with` "b" `inside` "c"                 `becomes` "c"
        replacing "a" `with` "b" `inside` "(λ a a)"           `becomes` "(λ b b)"
        replacing "a" `with` "b" `inside` "((λ a a) (λ b a))" `becomes` "((λ b b) (λ b b))"
