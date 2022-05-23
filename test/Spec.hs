import Data
import Eval
import Parser (run)
import Test.Hspec
import Test.Hspec.QuickCheck
import Control.Monad.Except (MonadError (throwError), liftM)

main :: IO ()
main = hspec $ do
  describe "parse" $ do
    it "TODO: not implemented" $
      1 `shouldBe` 1
      
    -- it "1" $
    --   run "1" `shouldBe` Right (Number 1)

    -- it "(+ 2 2)" $
    --   run "(+ 2 2)" `shouldBe` Right (List [Atom "+", Number 2, Number 2])

    -- it "(a b c)" $
    --   run "(a b c)" `shouldBe` Right (List [Atom "a", Atom "b", Atom "c"])

    -- it "(a (b (c)))" $ do
    --   run "(a (b (c)))" `shouldBe` Right (List [Atom "a", List [Atom "b", List [Atom "c"]]])

    -- it "(a '(quoted (dotted . list)) test)" $
    --   run "(a '(quoted (dotted . list)) test)" `shouldBe` Right (List [Atom "a", List [Atom "quote", List [Atom "quoted", DottedList [Atom "dotted"] (Atom "list")]], Atom "test"])