module Main where

import Control.Monad.Except (MonadError (throwError), liftM)
import Data
import Eval
import Lib
import Parser
import Repl
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne args