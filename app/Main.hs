{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Control.Lens
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Char
import Data.List
import qualified Data.Map.Lazy as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import System.Environment (getArgs)
import System.IO
import System.Process

main :: IO ()
main = do
  env <- loadEnv
  let hsc = runReader renderAll env
  putStrLn hsc
  return ()

loadEnv :: IO Env
loadEnv = do
  args <- getArgs
  -- Either get the first arg or read from stdin if not provided
  fileHandle <-
    if null args
      then return stdin
      else openFile (head args) ReadMode
  translationUnitDecl <- decodeFromHandle fileHandle
  let header = last $ getFilesInTU translationUnitDecl
  tdMap <- getTypedefsMap (return translationUnitDecl)
  let astNodes = getASTNodesFromFile header translationUnitDecl
  return (Env header tdMap astNodes)

data Env = Env
  { getFilePath :: FilePath,
    getTdMap :: M.Map String String,
    getASTNodes :: V.Vector ASTObject
  }
  deriving (Show)

renderAll :: Reader Env String
renderAll = do
  ast <- asks getASTNodes
  fp <- asks getFilePath
  let astObjs = V.toList ast
  renders <- mapM (renderASTObject fp) astObjs
  return (unlines renders)

-- * AST decoding

-- Small objects within AST nodes
data DeclType = DeclType
  { qualType :: String,
    desugaredQualType :: Maybe String
  }
  deriving (Show, Generic, FromJSON)

-- I think that location will probably be the most straightforward way
-- to match record fields and typedefs to anonymous structs.
data Loc = Loc
  { offset :: Maybe Int,
    line :: Maybe Int,
    col :: Maybe Int,
    tokLen :: Maybe Int,
    includedFrom :: Maybe IncludeFile
  }
  deriving (Show, Generic, FromJSON)

data IncludeFile = IncludeFile
  { file :: Maybe String
  }
  deriving (Show, Generic, FromJSON)

-- Only should get one of these at the top level
data TranslationUnitDecl = TranslationUnitDecl
  { syntaxTree :: V.Vector Value
  }
  deriving (Show)

instance FromJSON TranslationUnitDecl where
  parseJSON = withObject "TranslationUnitDecl" $ \obj -> do
    syntaxTree' <- obj .: fromString "inner"
    return (TranslationUnitDecl syntaxTree')

-- AST nodes i.e. any objects with "kind" field
data RecordDecl = RecordDecl
  { recordName :: Maybe String,
    loc :: Maybe Loc,
    fields :: Maybe (V.Vector ASTObject)
  }
  deriving (Show)

data FieldDecl = FieldDecl
  { fieldName :: String,
    fieldType :: DeclType
  }
  deriving (Show)

data ParmVarDecl = ParmVarDecl
  { parmVarName :: String,
    parmVarType :: DeclType
  }
  deriving (Show)

data FunctionDecl = FunctionDecl
  { name :: String,
    returnType :: DeclType,
    functionInner :: Maybe (V.Vector ASTObject)
  }
  deriving (Show)

data FullComment = FullComment
  { fullCommentInner :: Maybe (V.Vector ASTObject)
  }
  deriving (Show)

data EnumDecl = EnumDecl
  { enumName :: String,
    enumInner :: Maybe (V.Vector ASTObject)
  }
  deriving (Show)

data EnumConstantDecl = EnumConstantDecl
  { enumConstantName :: String
  }
  deriving (Show)

data TypedefDecl = TypedefDecl
  { typeDefName :: String,
    typeDefType :: DeclType
  }
  deriving (Show)

data Unimplemented = Unimplemented
  { kind :: String
  }
  deriving (Show)

data ASTObject
  = NodeRD RecordDecl
  | NodeFiD FieldDecl
  | NodeFuD FunctionDecl
  | NodePVD ParmVarDecl
  | NodeED EnumDecl
  | NodeECD EnumConstantDecl
  | NodeTD TypedefDecl
  | NodeUnimpl Unimplemented
  | NodeFullCmnt FullComment
  deriving (Show)

instance FromJSON ASTObject where
  parseJSON = withObject "ASTObject" $ \obj -> do
    kind <- obj .: fromString "kind"
    case kind of
      "RecordDecl" ->
        do
          recordName <- obj .:? fromString "name"
          loc <- obj .:? fromString "loc"
          fields <- obj .:? fromString "inner"
          return (NodeRD RecordDecl {recordName, loc, fields})
      "FieldDecl" ->
        do
          fieldName <- obj .: fromString "name"
          fieldType <- obj .: fromString "type"
          return (NodeFiD FieldDecl {fieldName, fieldType})
      "FunctionDecl" ->
        do
          name <- obj .: fromString "name"
          returnType <- obj .: fromString "type"
          functionInner <- obj .:? fromString "inner"
          return
            ( NodeFuD
                FunctionDecl
                  { name,
                    returnType,
                    functionInner
                  }
            )
      "ParmVarDecl" ->
        do
          parmVarName <- obj .: fromString "name"
          parmVarType <- obj .: fromString "type"
          return
            ( NodePVD
                ParmVarDecl
                  { parmVarName,
                    parmVarType
                  }
            )
      "EnumDecl" ->
        do
          enumName <- obj .: fromString "name"
          enumInner <- obj .: fromString "inner"
          return (NodeED EnumDecl {enumName, enumInner})
      "EnumConstantDecl" ->
        do
          enumConstantName <- obj .: fromString "name"
          return (NodeECD EnumConstantDecl {enumConstantName})
      "TypedefDecl" ->
        do
          typeDefName <- obj .: fromString "name"
          typeDefType <- obj .: fromString "type"
          return (NodeTD TypedefDecl {typeDefName, typeDefType})
      "FullComment" ->
        do
          fullCommentInner <- obj .:? fromString "inner"
          return (NodeFullCmnt FullComment {fullCommentInner})
      _ ->
        do return (NodeUnimpl Unimplemented {kind})

-- * Rendering

renderASTObject :: FilePath -> ASTObject -> Reader Env String
renderASTObject _ (NodeUnimpl _) = return ""
renderASTObject _ (NodeED ed) =
  return $ case enumInner ed of
    Nothing ->
      unlines
        [ "{{ enum",
          "    " <> enumName ed,
          "}}"
        ]
    Just x ->
      unlines
        ( [ "{{ enum",
            "    " <> enumName ed <> ","
          ]
            <> renderEnumConstantDecls x
            <> ["}}"]
        )
renderASTObject fp (NodeFuD fd) =
  do
    let funcName = name fd
    sig <-
      renderParamTypeSignature
        $ ( \case
              Just x -> V.toList x
              Nothing -> []
          )
        $ functionInner fd
    retType <- renderFunctionReturnType (qualType $ returnType fd)
    return $
      unlines
        [ "foreign import capi \"" <> fp <> " " <> funcName <> "\"",
          "    " <> funcName <> " :: " <> sig <> "IO (" <> retType <> ")"
        ]
renderASTObject fp (NodeRD rd) =
  case (recordName rd, fields rd) of
    (Just rn, Just f) ->
      do
        fields <- renderRecordFields f
        return $
          unlines
            ( [ "{{ struct",
                "    " <> fp <> ",",
                "    " <> rn <> ","
              ]
                <> fields
                <> ["}}"]
            )
    (Just rn, Nothing) ->
      return $
        unlines
          [ "{{ struct",
            "    " <> fp <> ",",
            "    " <> rn,
            "}}"
          ]
    (Nothing, _) -> return "!!Unimplemented: Anonymous struct!!"
renderASTObject _ (NodeTD _) = return ""
renderASTObject _ _ = return "!!Unimplemented AST Object!!"

renderRecordFields :: V.Vector ASTObject -> Reader Env [String]
renderRecordFields x =
  let u = V.map renderRecordField x
      v = sequence u
   in mapReader V.toList v

renderRecordField :: ASTObject -> Reader Env String
renderRecordField (NodeRD _) =
  return "!!Unimplemented Record Field: Nested RecordDecl!!"
renderRecordField (NodeFiD fd) = do
  t <- convertType (qualType $ fieldType fd)
  return
    ( "    "
        <> fieldName fd
        <> ", "
        <> t
        <> ","
    )
renderRecordField (NodeFullCmnt _) = return ""
renderRecordField x = return $ "!!Unimplemented Record Field: " <> show x <> "!!"

renderParamTypeSignature :: [ASTObject] -> Reader Env String
renderParamTypeSignature (NodePVD pv : xs) = do
  x <- renderParamTypeSignature xs
  t <- convertType (qualType (parmVarType pv))
  return (t <> " -> " <> x)
renderParamTypeSignature (_ : xs) = renderParamTypeSignature xs
renderParamTypeSignature [] = return ""

-- This is wrong!!! May be good enough for now but must fix eventually
renderFunctionReturnType :: String -> Reader Env String
renderFunctionReturnType = convertType . strip . takeWhile (/= '(')
  where
    strip = unwords . words

renderEnumConstantDecls :: V.Vector ASTObject -> [String]
renderEnumConstantDecls dV =
  -- Filter out comments for now
  let ecdV =
        V.filter
          ( \case
              (NodeECD _) -> True
              _ -> False
          )
          dV
   in V.toList $ V.map renderEnumConstantDecl ecdV

renderEnumConstantDecl :: ASTObject -> String
renderEnumConstantDecl (NodeECD ecd) =
  "    " <> enumConstantName ecd <> ","
renderEnumConstantDecl x =
  "!!Unimplemented: " <> show x <> "!!"

-- * Type conversion

basicTypeMap :: M.Map String String
basicTypeMap =
  M.fromList
    [ ("bool", "CBool"),
      ("_Bool", "CBool"),
      ("char", "CChar"),
      ("double", "CDouble"),
      ("float", "CFloat"),
      ("int", "CInt"),
      ("int32_t", "Int32"),
      ("long", "CLong"),
      ("size_t", "CSize"),
      ("uint16_t", "Word16"),
      ("uint32_t", "Word32"),
      ("unsigned int", "CUInt"),
      ("void", "()")
    ]

type FmtType = String -> String

addPtr :: FmtType
addPtr x = "Ptr (" <> x <> ")"

-- Capitalize the first alphanumeric character
structNameChange :: String -> String
structNameChange name =
  let (start, end) = T.break isAlpha $ T.pack name
   in case T.uncons end of
        Nothing -> name
        Just (x, xs) -> T.unpack $ start <> T.cons (toUpper x) xs

enumNameChange :: String -> String
enumNameChange = structNameChange

convertType :: String -> Reader Env String
convertType = tryTypeLookup id False

tryTypeLookup :: FmtType -> Bool -> String -> Reader Env String
tryTypeLookup nameChange isCompositeType t = do
  let strippedT = (T.unpack . T.strip . T.pack) t
  tdMap <- asks getTdMap
  case M.lookup strippedT basicTypeMap of
    Just x -> return (nameChange x)
    Nothing ->
      let tdResolvedT =
            (T.unpack . T.strip . T.pack) $
              fromMaybe strippedT $
                M.lookup strippedT tdMap
       in do
            (nameChange', t') <- tryConvertCompositeType nameChange tdResolvedT
            (nameChange'', t'') <- tryConvertPtrType nameChange' t'
            case (isCompositeType, t /= t', t' /= t'') of
              (True, False, False) -> return (nameChange' t')
              (True, _, _) -> tryTypeLookup nameChange'' True t''
              (False, False, False) -> return ("!!Unimplemented: " <> tdResolvedT <> "!!")
              (False, True, _) -> tryTypeLookup nameChange'' True t''
              (False, _, _) -> tryTypeLookup nameChange'' False t''

tryConvertCompositeType :: FmtType -> String -> Reader Env (FmtType, String)
tryConvertCompositeType nameChange t
  | "const " `isPrefixOf` t' = return (nameChange, drop 6 t')
  | "struct " `isPrefixOf` t' = return (nameChange . structNameChange, drop 7 t')
  | "enum " `isPrefixOf` t' = return (nameChange . enumNameChange, drop 5 t')
  | otherwise = return (nameChange, t)
  where
    t' = (T.unpack . T.strip . T.pack) t -- I will switch Strings to Text, eventually :)

tryConvertPtrType :: FmtType -> String -> Reader Env (FmtType, String)
tryConvertPtrType nameChange t = do
  case words t of
    [] -> return (nameChange, "TRY_CONVERT_PTR_TYPE_EMPTY")
    [_] -> return (nameChange, t)
    x : xs ->
      if "*" `isPrefixOf` head xs
        then return (addPtr . nameChange, x <> " " <> drop 1 (unwords xs))
        else return (nameChange, t)

-- * Process AST

-- Regular old 'fromJson' returns a Result type. I'm not too concerned
-- about the error portion, so just toss it and return a 'Maybe'.
decodeVals :: V.Vector Value -> V.Vector ASTObject
decodeVals x =
  let resultObjs = V.map (fromJSON :: Value -> Result ASTObject) x
      justSuccess =
        V.filter
          ( \case
              (Success _) -> True
              (Error _) -> False
          )
          resultObjs
      fromSuccess =
        ( \case
            (Success obj) -> obj
            (Error _) ->
              error
                "Error somehow bypassed filter" -- how do I avoid this
                -- nonsense? :/
        )
   in V.map fromSuccess justSuccess

-- First, filter out everything that isn't from the file in
-- question. Then parse and deal with the resultant Vector.
getASTNodesFromFile ::
  FilePath ->
  TranslationUnitDecl ->
  V.Vector ASTObject
getASTNodesFromFile fp tu =
  decodeVals $
    dropUntilFile fp (syntaxTree tu)

-- At the start of each file, the first ASTNode usually seems to have:
--
-- "loc: { file: x }".
--
-- I'm not sure how reliable this is, but for now I'm using this to
-- determine which header file is being bound to.
getLocFile :: (AsValue s) => s -> Maybe Value
getLocFile x =
  x ^? (_Value . key (fromString "loc") . key (fromString "file"))

getFilesInTU :: TranslationUnitDecl -> [String]
getFilesInTU TranslationUnitDecl {syntaxTree = vv} =
  let valueList = V.toList vv
      fileValues = mapMaybe getLocFile valueList
   in map (\(String x) -> T.unpack x) fileValues

dropUntilFile :: FilePath -> V.Vector Value -> V.Vector Value
dropUntilFile fp vv =
  let expectedFileLoc = Just (String (T.pack fp))
   in snd $ V.break (\x -> getLocFile x == expectedFileLoc) vv

-- * Typedefs

isTypedef :: ASTObject -> Bool
isTypedef (NodeTD _) = True
isTypedef _ = False

-- We want all of the typedefs in the translation unit, not just from
-- the current file.
getTypedefsFromTU :: TranslationUnitDecl -> V.Vector ASTObject
getTypedefsFromTU tu =
  V.filter isTypedef $
    decodeVals (syntaxTree tu)

typedefToTuple :: ASTObject -> (String, String)
typedefToTuple (NodeTD x) =
  let tdName = typeDefName x
      tdt = typeDefType x
      qt = qualType tdt
      dqt = desugaredQualType tdt
   in case dqt of
        Just t -> (tdName, t)
        Nothing -> (tdName, qt)
typedefToTuple _ = error "ASTObject isn't a typedef!"

getTypedefsMap :: IO TranslationUnitDecl -> IO (M.Map String String)
getTypedefsMap tud = typedefsMap . getTypedefsFromTU <$> tud

typedefsMap :: V.Vector ASTObject -> M.Map String String
typedefsMap = M.fromList . V.toList . V.map typedefToTuple

-- * Decode

decodeFromHandle :: Handle -> IO TranslationUnitDecl
decodeFromHandle h =
  do
    contents <- B.hGetContents h
    case decode (LB.fromStrict contents) of
      Just jsonValue -> return jsonValue
      Nothing -> return (TranslationUnitDecl (V.fromList []))

-- * Convenience functions for REPL use

_decodeASTNodes :: Handle -> FilePath -> IO (V.Vector ASTObject)
_decodeASTNodes h f =
  getASTNodesFromFile f <$> decodeFromHandle h

_decodeTypedefs :: Handle -> IO (V.Vector ASTObject)
_decodeTypedefs h =
  getTypedefsFromTU <$> decodeFromHandle h

_invokeClang :: [String] -> IO Handle
_invokeClang args =
  let args' =
        [ "-x",
          "c",
          "-Xclang",
          "-ast-dump=json",
          "-fsyntax-only"
        ]
          <> args
   in do
        (_, Just hout, _, _) <-
          createProcess
            (proc "clang" args')
              { std_out = CreatePipe,
                -- Suppress stdErr, which is often a bunch of useless
                -- garbage about not finding includes.
                --
                -- Might be useful though. May change eventually.
                std_err = CreatePipe
              }
        return hout
