{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module MyLib (everything) where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.Lens
import System.Process
import Control.Lens
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

-- Small objects within AST nodes
data DeclType where
  DeclType :: {qualType :: String} -> DeclType
  deriving (Show, Generic, FromJSON)

-- AST nodes i.e. any objects with "kind" field
data TranslationUnitDecl = TranslationUnitDecl
  { syntaxTree :: V.Vector Value
  } deriving Show

data RecordDecl = RecordDecl
  { recordName :: Maybe String
  , fields :: Maybe (V.Vector ASTObject)
  } deriving Show

data FieldDecl = FieldDecl
  { fieldName :: String
  , fieldType :: DeclType
  } deriving Show

data ParmVarDecl = ParmVarDecl
  { parmVarName :: String
  , parmVarType :: DeclType
  } deriving Show

data FunctionDecl = FunctionDecl
  { name :: String
  , returnType :: DeclType
  , functionInner :: Maybe (V.Vector ASTObject)
  } deriving Show

data FullComment = FullComment
  { fullCommentInner :: Maybe (V.Vector ASTObject)
  } deriving Show

data EnumDecl = EnumDecl
  { enumName :: String
  , enumInner :: Maybe (V.Vector ASTObject)
  } deriving Show

data EnumConstantDecl = EnumConstantDecl
  { enumConstantName :: String
  } deriving Show

data Unimplmented = Unimplmented
  { kind :: String
  } deriving Show

data ASTObject = NodeTUD TranslationUnitDecl
               | NodeRD RecordDecl
               | NodeFiD FieldDecl
               | NodeFuD FunctionDecl
               | NodePVD ParmVarDecl
               | NodeED EnumDecl
               | NodeECD EnumConstantDecl
               | NodeUnimpl Unimplmented
               | NodeFullCmnt FullComment
               deriving Show
instance FromJSON ASTObject where
  parseJSON = withObject "ASTObject" $ \obj -> do
    kind' <- obj .: fromString "kind"
    case kind' of
      "TranslationUnitDecl"
        -> do syntaxTree' <- obj .: fromString "inner"
              return (NodeTUD TranslationUnitDecl
                      { syntaxTree = syntaxTree' })
      "RecordDecl"
        -> do recordName' <- obj .:? fromString "name"
              fields' <- obj .:? fromString "inner"
              return (NodeRD RecordDecl
                      { recordName = recordName'
                      , fields = fields' })
      "FieldDecl"
        -> do fieldName' <- obj .: fromString "name"
              fieldType' <- obj .: fromString "type"
              return (NodeFiD FieldDecl
                      { fieldName = fieldName'
                      , fieldType = fieldType' })
      "FunctionDecl"
        -> do name' <- obj .: fromString "name"
              returnType' <- obj .: fromString "type"
              functionInner' <- obj .:? fromString "inner"
              return (NodeFuD FunctionDecl
                      { name = name'
                      , returnType = returnType'
                      , functionInner = functionInner' })
      "ParmVarDecl"
        -> do parmVarName' <- obj .: fromString "name"
              parmVarType' <- obj .: fromString "type"
              return (NodePVD ParmVarDecl
                      { parmVarName = parmVarName'
                      , parmVarType = parmVarType' })
      "EnumDecl"
        -> do enumName' <- obj .: fromString "name"
              enumInner' <- obj .: fromString "inner"
              return (NodeED EnumDecl
                      { enumName = enumName'
                      , enumInner = enumInner' })
      "EnumConstantDecl"
        -> do enumConstantName' <- obj .: fromString "name"
              return (NodeECD EnumConstantDecl
                      { enumConstantName = enumConstantName' })
      "FullComment"
        -> do fullCommentInner' <- obj .:? fromString "inner"
              return (NodeFullCmnt FullComment
                     { fullCommentInner = fullCommentInner' })
      _
        -> do return (NodeUnimpl Unimplmented { kind = kind' })


renderASTObject :: FilePath -> ASTObject -> String
renderASTObject _  (NodeTUD _) = ""
renderASTObject _  (NodeUnimpl _) = ""
renderASTObject fp (NodeED ed) =
  case enumInner ed of
    Nothing
      -> unlines
         [ "{{ enum"
         , "    "++enumName ed
         , "}}"
         ]
    Just x
      -> unlines
         ([ "{{ enum"
          , "    "++enumName ed++","
          ]++renderEnumConstantDecls x++["}}"])
renderASTObject fp (NodeFuD fd) =
  let funcName = name fd
      sig = renderParamTypeSignature (getInnerAsList $ functionInner fd)
      retType = renderFunctionReturnType (qualType $ returnType fd)
  in unlines
     [ "foreign import capi \""++fp++" "++funcName++"\""
     , "    "++funcName++" :: "++sig++"IO ("++retType++")"
     ]
renderASTObject fp (NodeRD rd) =
  let (Just rn) = recordName rd
  in case (fields rd) of
    Nothing
      -> unlines
         [ "{{ struct"
         , "    "++fp++","
         , "    "++rn
         , "}}"
         ]
    Just x
      -> unlines
         ([ "{{ struct"
          , "    "++fp++","
          , "    "++rn++","
          ]++renderRecordFields x++[ "}}"])
renderASTObject _ _ = "!!Unimplemented AST Object!!"

renderRecordFields :: V.Vector ASTObject -> [String]
renderRecordFields x = V.toList $ V.map renderRecordField x

renderRecordField :: ASTObject -> String
renderRecordField (NodeRD rd) = "!!Unimplmented Record Field: Nested RecordDecl!!"
renderRecordField (NodeFiD fd) = fieldName fd++", "++convertType (qualType $ fieldType fd)++","
renderRecordField (NodeFullCmnt _) = ""
renderRecordField x = "!!Unimplemented Record Field: "++show x++"!!"

getInnerAsList :: Maybe (V.Vector ASTObject) -> [ASTObject]
getInnerAsList (Just x) = V.toList x
getInnerAsList Nothing  = []

renderParamTypeSignature :: [ASTObject] -> String
renderParamTypeSignature (NodePVD pv:xs) = convertType (qualType $ parmVarType pv)++" -> "++renderParamTypeSignature xs
renderParamTypeSignature (_:xs)          = renderParamTypeSignature xs
renderParamTypeSignature []              = ""

-- This is wrong!!! May be good enough for now but must fix eventually
renderFunctionReturnType :: String -> String
renderFunctionReturnType = convertType . strip . takeWhile (/= '(')
  where strip = unwords . words

renderEnumConstantDecls :: V.Vector ASTObject -> [String]
renderEnumConstantDecls d =
  -- Filter out comments for now
  let ecdV = V.filter (\case
                          (NodeECD _) -> True
                          _           -> False) d
  in V.toList $ V.map renderEnumConstantDecl ecdV

renderEnumConstantDecl :: ASTObject -> String
renderEnumConstantDecl (NodeECD ecd) = "    "++enumConstantName ecd++","
renderEnumConstantDecl x             = "!!Unimplemented: "++show x++"!!"

convertType :: String -> String
convertType "bool"       = "CBool"
convertType "int"        = "CInt"
convertType "int *"      = "Ptr CInt"
convertType "size_t"     = "CSize"
convertType "size_t *"   = "Ptr CSize"
convertType "uint16_t"   = "Word16"
convertType "uint16_t *" = "Ptr Word16"
convertType "uint32_t"   = "Word32"
convertType "uint32_t *" = "Ptr Word32"
convertType "void"       = ""
convertType "void *"     = "Ptr ()"
convertType "void **"    = "Ptr (Ptr ())"
convertType ('s':'t':'r':'u':'c':'t':xs) =
  case words xs of
  [x, "*"] -> "Ptr "++structNameChange x
  [x]      -> structNameChange x
  _        -> "!!Unimplemented struct type: struct"++xs++"!!"
convertType x = "!!Unimplimented: "++x++"!!"

-- TODO: Capitalize first thing before _
structNameChange :: String -> String
structNameChange = id

-- TODO: Parameterize this; users should be able to pass in absolute paths.
clangExecutable :: String
clangExecutable = "clang"

invokeClang :: [String] -> IO ASTObject
invokeClang args =
  let
    args' = [ "-x"
            , "c"
            , "-Xclang"
            , "-ast-dump=json"
            , "-fsyntax-only"
            ] ++ args
  in
    do
      (_, Just hout, _, _) <-
        createProcess (proc clangExecutable args') { std_out = CreatePipe
                                                  -- suppress stdErr, which is
                                                  -- usually a bunch of garbage
                                                  -- about not finding includes
                                                  -- 
                                                  -- might be useful
                                                  -- though. change eventually
                                                  , std_err = CreatePipe
                                                  }
      contents <- B.hGetContents hout
      let (Just jsonValue) = decode (LB.fromStrict contents)
      return jsonValue

-- Don't parse the nodes inside of the syntax tree just yet. First, filter out
-- everything that isn't from the file in question. Then parse and deal with
-- the resultant Vector.
getASTNodesFromFile :: FilePath -> ASTObject -> Maybe (V.Vector ASTObject)
getASTNodesFromFile fp (NodeTUD tu) =
  let
    getFileLoc = (_Value . key (fromString "loc") . key (fromString "file"))
    expectedFileLoc = Just (String (T.pack fp))
    -- run fromJSON inside of Maybe Vector and change Result to Maybe
    convertVals x =
      let resultObjs = V.map (fromJSON :: Value -> Result ASTObject) x
      in case sequence resultObjs of
           Success objs -> Just objs
           Error _      -> Nothing
  in
    convertVals $ snd
    $ V.break (\x -> x ^? getFileLoc == expectedFileLoc) (syntaxTree tu)
getASTNodesFromFile _ _ = Nothing

invokeAndGetASTNodes :: [String] -> IO (Maybe (V.Vector ASTObject))
invokeAndGetASTNodes args = getASTNodesFromFile (last args) <$> invokeClang args

renderAll :: FilePath -> IO (Maybe (V.Vector ASTObject)) -> IO String
renderAll fp ast = do
  astObjs <- getInnerAsList <$> ast
  let renders = map (renderASTObject fp) astObjs
  return (unlines renders)

everything :: [String] -> IO String
everything x =
  let fp = last x
  in renderAll fp (invokeAndGetASTNodes x)
