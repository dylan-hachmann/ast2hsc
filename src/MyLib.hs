{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module MyLib where

import Control.Lens
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Lazy as M
import qualified Data.Text as T
import qualified Data.Vector as V
import GHC.Generics
import System.Process

-- Small objects within AST nodes
data DeclType = DeclType
  { qualType :: String,
    desugaredQualType :: Maybe String
  }
  deriving (Show, Generic, FromJSON)

-- Location for typedef-ing anonymous structs
data Loc = Loc
  { offset :: Maybe Int,
    -- I think this one could be a Maybe...but let the
    -- program crash for now and deal with it if it
    -- becomes a problem
    line :: Maybe Int,
    col :: Maybe Int,
    tokLen :: Maybe Int,
    includedFrom :: Maybe IncludeFile
  }
  deriving (Show, Generic, FromJSON)

-- Annoyingly, this is nested, but I've never seen a single thing
-- besides "file" in there.
data IncludeFile = IncludeFile
  { file :: Maybe String
  }
  deriving (Show, Generic, FromJSON)

-- Only should get one of these at the top level, so it can be thought
-- of as an intermediate format that signifies a successful parse.
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

renderASTObject :: FilePath -> ASTObject -> Reader Env String
renderASTObject _ (NodeUnimpl _) = return ""
renderASTObject _ (NodeED ed) =
  return $ case enumInner ed of
    Nothing ->
      unlines
        [ "{{ enum",
          "    " ++ enumName ed,
          "}}"
        ]
    Just x ->
      unlines
        ( [ "{{ enum",
            "    " ++ enumName ed ++ ","
          ]
            ++ renderEnumConstantDecls x
            ++ ["}}"]
        )
renderASTObject fp (NodeFuD fd) =
  do
    let funcName = name fd
    sig <-
      renderParamTypeSignature
        $ ( \case
              Just x -> getInnerAsList x
              Nothing -> []
          )
        $ functionInner fd
    retType <- renderFunctionReturnType (qualType $ returnType fd)
    return $
      unlines
        [ "foreign import capi \"" ++ fp ++ " " ++ funcName ++ "\"",
          "    " ++ funcName ++ " :: " ++ sig ++ "IO (" ++ retType ++ ")"
        ]
renderASTObject fp (NodeRD rd) =
  case (recordName rd, fields rd) of
    (Just rn, Just f) ->
      do
        fields <- renderRecordFields f
        return $
          unlines
            ( [ "{{ struct",
                "    " ++ fp ++ ",",
                "    " ++ rn ++ ","
              ]
                ++ fields
                ++ ["}}"]
            )
    (Just rn, Nothing) ->
      return $
        unlines
          [ "{{ struct",
            "    " ++ fp ++ ",",
            "    " ++ rn,
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
        ++ fieldName fd
        ++ ", "
        ++ t
        ++ ","
    )
renderRecordField (NodeFullCmnt _) = return ""
renderRecordField x = return $ "!!Unimplemented Record Field: " ++ show x ++ "!!"

getInnerAsList :: V.Vector ASTObject -> [ASTObject]
getInnerAsList = V.toList

renderParamTypeSignature :: [ASTObject] -> Reader Env String
renderParamTypeSignature (NodePVD pv : xs) = do
  x <- renderParamTypeSignature xs
  t <- convertType (qualType (parmVarType pv))
  return (t ++ " -> " ++ x)
renderParamTypeSignature (_ : xs) = renderParamTypeSignature xs
renderParamTypeSignature [] = return $ ""

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
  "    " ++ enumConstantName ecd ++ ","
renderEnumConstantDecl x =
  "!!Unimplemented: " ++ show x ++ "!!"

convertType :: String -> Reader Env String
convertType "char" = return "CChar"
convertType "char *" = return "Ptr CChar"
convertType "bool" = return "CBool"
convertType "_Bool" = return "CBool"
convertType "long" = return "CLong"
convertType "double" = return "CDouble"
convertType "float" = return "CFloat"
convertType "int" = return "CInt"
convertType "int *" = return "Ptr CInt"
convertType "size_t" = return "CSize"
convertType "size_t *" = return "Ptr CSize"
convertType "unsigned int" = return "CUInt"
convertType "uint16_t" = return "Word16"
convertType "uint16_t *" = return "Ptr Word16"
convertType "int32_t" = return "Int32"
convertType "uint32_t" = return "Word32"
convertType "uint32_t *" = return "Ptr Word32"
convertType "void" = return ""
convertType "void *" = return "Ptr ()"
convertType "void **" = return "Ptr (Ptr ())"
convertType ('s' : 't' : 'r' : 'u' : 'c' : 't' : xs) =
  return $ case words xs of
    [x, "*"] -> "Ptr " ++ structNameChange x
    [x, "**"] -> "Ptr (Ptr " ++ structNameChange x ++ ")"
    [x] -> structNameChange x
    _ -> "!!Unimplemented struct type: struct" ++ xs ++ "!!"
convertType ('e' : 'n' : 'u' : 'm' : xs) =
  return $ case words xs of
    [x, "*"] -> "Ptr " ++ enumNameChange x
    [x] -> enumNameChange x
    _ -> "!!Unimplemented enum type: enum" ++ xs ++ "!!"
convertType ('c' : 'o' : 'n' : 's' : 't' : ' ' : xs) = convertType xs
convertType x = do
  tdMap <- asks getTdMap
  let x' = M.lookup x tdMap
  case x' of
    Nothing -> return $ "!!Unimplemented: " ++ x ++ "!!"
    Just x'' ->
      if x'' /= x
        then convertType x''
        else return $ "!!Unimplemented: " ++ x'' ++ "!!"

-- TODO: Capitalize first thing before _
structNameChange :: String -> String
structNameChange = id

-- TODO: Capitalize first thing before _
enumNameChange :: String -> String
enumNameChange = id

-- TODO: Parameterize this; users should be able to pass in absolute
-- paths.
clangExecutable :: String
clangExecutable = "clang"

invokeClang :: [String] -> IO TranslationUnitDecl
invokeClang args =
  let args' =
        [ "-x",
          "c",
          "-Xclang",
          "-ast-dump=json",
          "-fsyntax-only"
        ]
          ++ args
   in do
        (_, Just hout, _, _) <-
          createProcess
            (proc clangExecutable args')
              { std_out = CreatePipe,
                -- suppress stdErr, which is usually a bunch of garbage about
                -- not finding includes
                --
                -- might be useful though. change eventually
                std_err = CreatePipe
              }
        contents <- B.hGetContents hout
        case decode (LB.fromStrict contents) of
          Just jsonValue -> return jsonValue
          Nothing -> return (TranslationUnitDecl (V.fromList []))

-- Regular old 'fromJson' returns a Result type. I'm not too concerned
-- about the error portion, so just toss it and return a 'Maybe'.
convertVals :: V.Vector Value -> V.Vector ASTObject
convertVals x =
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
                "Error somehow bypassed filter" --how do I avoid this
                                                --nonsense? :/
        )
   in V.map fromSuccess justSuccess

-- Don't parse the nodes inside of the syntax tree just yet. First,
-- filter out everything that isn't from the file in question. Then
-- parse and deal with the resultant Vector.
getASTNodesFromFile ::
  FilePath ->
  TranslationUnitDecl ->
  V.Vector ASTObject
getASTNodesFromFile fp tu =
  convertVals $
    dropUntilFile fp (syntaxTree tu)

-- Drop nodes
dropUntilFile :: FilePath -> V.Vector Value -> V.Vector Value
dropUntilFile fp vv =
  let getFileLoc =
        ( _Value
            . key (fromString "loc")
            . key (fromString "file")
        )
      expectedFileLoc = Just (String (T.pack fp))
   in snd $ V.break (\x -> x ^? getFileLoc == expectedFileLoc) vv

isTypedef :: ASTObject -> Bool
isTypedef (NodeTD _) = True
isTypedef _ = False

-- We want all of the typedefs in the translation unit, not just from
-- the current file.
getTypedefsFromTU :: TranslationUnitDecl -> V.Vector ASTObject
getTypedefsFromTU tu =
  V.filter isTypedef $
    convertVals (syntaxTree tu)

invokeAndGetASTNodes :: [String] -> IO (V.Vector ASTObject)
invokeAndGetASTNodes args =
  getASTNodesFromFile (last args)
    <$> invokeClang args

invokeAndGetTypedefs :: [String] -> IO (V.Vector ASTObject)
invokeAndGetTypedefs args =
  getTypedefsFromTU
    <$> invokeClang args

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
getTypedefsMap tud = do
  x <- tud
  return (typedefsMap $ getTypedefsFromTU x)

typedefsMap :: V.Vector ASTObject -> M.Map String String
typedefsMap = M.fromList . V.toList . V.map typedefToTuple

-- Go through every typedef in a map until the "base" type is
-- reached. Imaginably, this is usually one or zero iterations to
-- either resolve a typedef or not, but this method provides support
-- nested typedefs if they are to occur.
unfurlTypedefs :: M.Map String String -> DeclType -> DeclType
unfurlTypedefs tdMap dt =
  let qt = qualType dt
      dqt = desugaredQualType dt
      t = case dqt of
        Just x -> x
        Nothing -> qt
      resolvedType = M.lookup t tdMap
   in case resolvedType of
        Just rt -> unfurlTypedefs tdMap $ DeclType rt Nothing
        Nothing -> DeclType t Nothing

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
  let astObjs = getInnerAsList ast
  renders <- mapM (renderASTObject fp) astObjs
  return (unlines renders)

_clangArgs :: [String]
_clangArgs =
  [ "-DWLR_USE_UNSTABLE",
    "-I/home/dylan/Development/wlroots/tinywl",
    "-I/gnu/store/7vs22p1alhs721lfv3v22h09md6gsxb8-wayland-1.22.0/include",
    "-I/gnu/store/zdn2b0glrrfw08dzif3ziijw2rl1jfj2-libxkbcommon-1.6.0/include",
    "-I/gnu/store/9hs4p558s17yvl9rw9ky5ygfbmc4jdgc-pixman-0.42.2/include/pixman-1",
    "-I/gnu/store/acf5nffb6f1z3x5q0b3i1pwmzkvx51sc-mesa-24.0.4/include",
    "-I/gnu/store/d2w7iyqzkgwkyn9cmz8asna4k7a26gs2-libxfixes-6.0.0/include",
    "-I/gnu/store/yb81b92lsn0aixvnz1qw5x8xs05gj3dz-libx11-1.8.7/include",
    "-I/gnu/store/amzj6l8yr8llh3gqnkdskmrwqh31krjl-libxcb-1.15/include",
    "-I/gnu/store/qsjkhchjhlca4gms9b4v43afb0mrw2fa-libxxf86vm-1.1.4/include",
    "-I/gnu/store/71y51h0kyd2hp1yfhfmkmlpaqi326c6q-libxext-1.3.4/include",
    "-I/gnu/store/yb81b92lsn0aixvnz1qw5x8xs05gj3dz-libx11-1.8.7/include",
    "-I/gnu/store/kld1hhkfi8v61bmjvqbm0igp1a6dww4q-libdrm-2.4.120/include",
    "-I/gnu/store/kld1hhkfi8v61bmjvqbm0igp1a6dww4q-libdrm-2.4.120/include/libdrm",
    "-I/gnu/store/acf5nffb6f1z3x5q0b3i1pwmzkvx51sc-mesa-24.0.4/include",
    "-I/gnu/store/j1yqdjchjj2md4r3cfkmbpxkcva5ahqy-elogind-252.9/include/elogind",
    "-I/gnu/store/7vs22p1alhs721lfv3v22h09md6gsxb8-wayland-1.22.0/include",
    "-I/gnu/store/1lzfbbwcpyngm81v44fr34253b8is7zr-libffi-3.4.4/include",
    "-I/gnu/store/amzj6l8yr8llh3gqnkdskmrwqh31krjl-libxcb-1.15/include",
    "-I/gnu/store/482998zixfp5nkb1dyf5znj4vvqmkh8n-libxau-1.0.10/include",
    "-I/gnu/store/4h67m2s2nsy0a8hp0miwz3mpjn6rxb15-libxdmcp-1.1.3/include",
    "-I/gnu/store/xsy1q53sb5iv1mp7ihv3il4n7nbvgbqr-xorgproto-2023.2/include",
    "-I/gnu/store/4lfxl1c944fn4jw7306y05nwi10jmgl3-wlroots-0.17.1/include",
    "-I/gnu/store/66xp495ibhhnc3f95f6z8n97j176h8mm-eudev-3.2.14/include",
    "-I/gnu/store/75zsndb0g9z4700yrhwbfyaxins1sqad-libseat-0.7.0/include",
    "-I/gnu/store/6cab9rrsi5zwqfn2biylibqrmzbnbvfw-libdisplay-info-0.2.0-dev-0.ebee359/include",
    "-I/gnu/store/dka9hd1jgbxpfxy94mslhfbcv7b94fis-libinput-minimal-1.22.1/include",
    "-I/gnu/store/vmkvapcpppnd3xzqqy3qpi3kcpslv47q-xcb-util-wm-0.4.1/include",
    "-I/gnu/store/j49knm9grnjfin9hzhqzial405jpxybg-xcb-util-errors-1.0-1.5d660eb/include",
    "-L/gnu/store/4lfxl1c944fn4jw7306y05nwi10jmgl3-wlroots-0.17.1/lib",
    "-lwlroots",
    "-I/gnu/store/7vs22p1alhs721lfv3v22h09md6gsxb8-wayland-1.22.0/include",
    "-I/gnu/store/1lzfbbwcpyngm81v44fr34253b8is7zr-libffi-3.4.4/include",
    "-L/gnu/store/7vs22p1alhs721lfv3v22h09md6gsxb8-wayland-1.22.0/lib",
    "-lwayland-server",
    "-I/gnu/store/zdn2b0glrrfw08dzif3ziijw2rl1jfj2-libxkbcommon-1.6.0/include",
    "-L/gnu/store/zdn2b0glrrfw08dzif3ziijw2rl1jfj2-libxkbcommon-1.6.0/lib",
    "-lxkbcommon",
    "../wlroots/include/wlr/types/wlr_output.h"
  ]

_testTypedefsArgs =
  ["test/typedefs.h"]
