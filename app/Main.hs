module Main where

import Control.Monad.Trans.Reader
import qualified Data.Map as M
import qualified Data.Vector as V
import MyLib
import System.Environment (getArgs)
import System.IO
import Text.Pretty.Simple

main :: IO ()
main = do
  env <- loadEnv
  let str = runReader renderAll env
  putStrLn str
  return ()

loadEnv :: IO Env
loadEnv = do
  translationUnitDecl <- decodeFromHandle stdin
  let header = last $ getFilesInTU translationUnitDecl
  tdMap <- getTypedefsMap (return translationUnitDecl)
  let astNodes = getASTNodesFromFile header translationUnitDecl
  return (Env header tdMap astNodes)
