module Main where

import Control.Monad.Trans.Reader
import qualified Data.Map as M
import qualified Data.Vector as V
import MyLib
import System.Environment (getArgs)
import Text.Pretty.Simple

data Env = Env
  { getTdMap :: M.Map String String,
    getASTNodes :: V.Vector ASTObject
  }
  deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  translationUnitDecl <- invokeClang args
  tdMap <- getTypedefsMap (return translationUnitDecl)
  let header = last args
      astNodes = getASTNodesFromFile header translationUnitDecl
      env = Env tdMap astNodes
  putStrLn (show env)
  return ()
