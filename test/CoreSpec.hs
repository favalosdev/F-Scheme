module CoreSpec (spec) where

import FScheme.Core.Environment
import FScheme.Core.Evaluator
import Test.Hspec

spec :: Spec
spec = describe "core Tests" $ do
  it "(+ 1 2)" $ do
    env <- primitiveBindings
    result <- evalString env "(+ 1 2)"
    result `shouldBe` "3"

  it "(- 5 3)" $ do
    env <- primitiveBindings
    result <- evalString env "(- 5 3)"
    result `shouldBe` "2"

  it "(* 4 6)" $ do
    env <- primitiveBindings
    result <- evalString env "(* 4 6)"
    result `shouldBe` "24"

  it "(/ 10 2)" $ do
    env <- primitiveBindings
    result <- evalString env "(/ 10 2)"
    result `shouldBe` "5"

  it "(+ (* 2 3) 4)" $ do
    env <- primitiveBindings
    result <- evalString env "(+ (* 2 3) 4)"
    result `shouldBe` "10"

  it "(- (* 5 6) 7)" $ do
    env <- primitiveBindings
    result <- evalString env "(- (* 5 6) 7)"
    result `shouldBe` "23"

  it "(* (+ 1 2) (- 4 1))" $ do
    env <- primitiveBindings
    result <- evalString env "(* (+ 1 2) (- 4 1))"
    result `shouldBe` "9"

  it "(and #t #t)" $ do
    env <- primitiveBindings
    result <- evalString env "(and #t #t)"
    result `shouldBe` "#t"

  it "(or #f #t)" $ do
    env <- primitiveBindings
    result <- evalString env "(or #f #t)"
    result `shouldBe` "#t"

  it "(not #f)" $ do
    env <- primitiveBindings
    result <- evalString env "(not #f)"
    result `shouldBe` "#t"

  it "(not #t)" $ do
    env <- primitiveBindings
    result <- evalString env "(not #t)"
    result `shouldBe` "#f"

  it "(= 5 5)" $ do
    env <- primitiveBindings
    result <- evalString env "(= 5 5)"
    result `shouldBe` "#t"

  it "(< 3 4)" $ do
    env <- primitiveBindings
    result <- evalString env "(< 3 4)"
    result `shouldBe` "#t"

  it "(> 6 5)" $ do
    env <- primitiveBindings
    result <- evalString env "(> 6 5)"
    result `shouldBe` "#t"

  it "(<= 4 4)" $ do
    env <- primitiveBindings
    result <- evalString env "(<= 4 4)"
    result `shouldBe` "#t"

  it "(>= 7 6)" $ do
    env <- primitiveBindings
    result <- evalString env "(>= 7 6)"
    result `shouldBe` "#t"
