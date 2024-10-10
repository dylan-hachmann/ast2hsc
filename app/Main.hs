module Main where

import Control.Monad.Trans.Reader
import qualified Data.Map as M
import qualified Data.Vector as V
import MyLib
import System.Environment (getArgs)
import Text.Pretty.Simple

main :: IO ()
main = do
  env <- loadEnv
  let str = runReader renderAll env :: String
  print str
  return ()

loadEnv :: IO Env
loadEnv = do
  args <- getArgs
  let filepath = last args
  translationUnitDecl <- invokeClang args
  tdMap <- getTypedefsMap (return translationUnitDecl)
  let header = last args
      astNodes = getASTNodesFromFile header translationUnitDecl
  return (Env filepath tdMap astNodes)
