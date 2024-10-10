module Main where

import qualified MyLib (everything)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  all <- MyLib.everything args
  putStrLn all
