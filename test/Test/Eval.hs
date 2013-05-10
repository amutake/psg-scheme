module Test.Eval where

import Data.IORef.Lifted
import Test.Hspec

import Eval
import Initial
import Types

runEvalTests :: IO ()
runEvalTests = hspec $ do
    evalLambdaTest

evalLambdaTest :: Spec
evalLambdaTest = do
    describe "eval lambda" $ do
        it "doesn't throw any exception" $ do
            ref <- newIORef initialEnv
            evalList ref add1 `shouldReturn` lambda ref
  where
    add1 = ProperList [Ident "lambda", List (ProperList [Ident "x"]), List (ProperList [Ident "+", Ident "x", Number 1])]
    lambda ref = Func (Lambda (ProperList ["x"]) [(List (ProperList [Ident "+", Ident "x", Number 1]))] ref)
