module Main where

import qualified MyLib (everything, invokeClang)
import Text.Pretty.Simple
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case (head args) of
    "parsedAST" -> do
      astObj <- MyLib.invokeClang (tail args)
      pPrint astObj
    _           -> do
      renderedContent <- MyLib.everything args
      putStrLn renderedContent
