{-# OPTIONS_GHC -Wall #-}

module Compiler where

import           Control.Monad       (forever)
import           Control.Concurrent
import qualified Data.List           as List
import qualified Data.Map            as Map
import           Data.Maybe

import           Elm.Compiler as EC
import           Elm.Compiler.Module as ECM
import           Elm.Package

import qualified Utils.File

-- import AST.Declaration import AST.Variable import AST.Module import AST.Module.Name import
-- AST.Type
{-

In order to run the compiler we need several pieces of data:

-- Elm.Package
data Name = Name { user :: String, project :: String }
data Version = Version { _major :: Int, _minor :: Int, _patch :: Int }
type Package = (Name, Version)

-- Elm.Compiler.Module
data Interface = Interface {
    iVersion  :: Package.Version,
    iPackage  :: Package.Name,
    iExports  :: [Var.Value],
    iImports  :: [AST.Module.Name.Raw],
    iTypes    :: Types,
    iUnions   :: Unions,
    iAliases  :: Aliases,
    iFixities :: [AST.Declaration.Infix]
}
type Interfaces = Map AST.Module.Name.Canonical Interface

-- AST.Declaration
data Assoc = L | N | R
data Infix = Infix { _op :: String, _associativity :: Assoc, _precedence :: Int }

-- AST.Variable
data Listing a = Listing
    { _explicits :: [a]
    , _open :: Bool
    }
data Value
    = Value !String
    | Alias !String
    | Union !String !(Listing String)

-- AST.Module
type Types = Map String Type.Canonical
type Unions = Map String (UnionInfo String)
type Aliases = Map String ([String], Type.Canonical)
type UnionInfo = ( [String], [(v, [Type.Canonical])] )

-- AST.Module.Name
type Raw = [String]
data Canonical = Canonical { _package :: Package.Name, _module :: Raw }

-- AST.Type
data Canonical
    = Lambda Canonical Canonical
    | Var String
    | Type Var.Canonical
    | App Canonical [Canonical]
    | Record [(String, Canonical)] (Maybe Canonical)
    | Aliased Var.Canonical [(String, Canonical)] (Aliased Canonical)

-- Elm.Compiler
data Context = Context {
    _packageName :: Package.Name,
    _isExposed :: bool,
    _dependencies :: [AST.Module.Name.Canonical]
}

compile :: Context -> String -> Elm.Package.Interfaces -> *stuff* :-)

-}

-- Modules matched to the default imports
importModules :: [ECM.Raw]
importModules = [ ["Basics"]
                , ["Debug"]
                , ["List"]
                , ["Maybe"]
                , ["Result"]
                , ["Platform"]
                , ["Platform", "Cmd"]
                , ["Platform", "Sub"]
                ]

moduleVersions =
  [
    (["Basics"], ((ECM.Canonical (Name "elm-lang" "core") ["Basics"]), "4.0.0")),
    (["List"], ((ECM.Canonical (Name "elm-lang" "core") ["List"]), "4.0.0")),
    (["Maybe"], ((ECM.Canonical (Name "elm-lang" "core") ["Maybe"]), "4.0.0")),
    (["Result"], ((ECM.Canonical (Name "elm-lang" "core") ["Result"]), "4.0.0")),
    (["Platform"], ((ECM.Canonical (Name "elm-lang" "core") ["Platform"]), "4.0.0")),
    (["Platform","Cmd"], ((ECM.Canonical (Name "elm-lang" "core") ["Platform","Cmd"]), "4.0.0")),
    (["Platform","Sub"], ((ECM.Canonical (Name "elm-lang" "core") ["Platform","Sub"]), "4.0.0"))
  ]

-- A function to yield a pair of canonical name and binary interface from a canonical module
-- name
readInterface ::
  String ->
  (ECM.Canonical, String) ->
  IO (ECM.Canonical, ECM.Interface)
readInterface versionString (name, version) =
  -- A function to make the hyphenated version of a package name as in build-artifacts
  let hyphenate rawName = List.intercalate "-" rawName
  in
  -- A function that builds the relative file name in elm-stuff of an elmi file. We cheat on
  -- the version number.
  let fileName (ECM.Canonical (Name user project) modPath) = List.intercalate "/"
           [ "elm-stuff"
           , "build-artifacts"
           , versionString
           , user
           , project
           , version
           , (hyphenate
                modPath) ++ ".elmi"
           ]
  in
  do
      filename <- pure $ fileName name
      interface <- Utils.File.readBinary filename
      return $ (name, interface)

readInterfaceService ::
  String ->
  Chan [(ECM.Canonical, String)] ->
  Chan [(ECM.Canonical, ECM.Interface)] ->
  IO ()
readInterfaceService versionString requestChan replyChan =
  forever $ do
    requestedInterfaces <- readChan requestChan
    -- Load up the interfaces into an IO [(Canonical, Interface)]
    retrievedInterfaces <- mapM (readInterface versionString) requestedInterfaces
    writeChan replyChan retrievedInterfaces

moduleRequestList moduleVersions parseResult =
  -- Make a list of the names of modules we need to import
  case parseResult of
    Left errors -> ([],[])
    Right (_, myname, imports) ->
      let singletonLookup rawName = maybeToList $ lookup rawName moduleVersions in
      let allNames = List.union imports importModules in
      (myname, List.concatMap singletonLookup allNames)

compileCodeService ::
  String ->
  [(ECM.Raw, (ECM.Canonical, String))] ->
  Chan String ->
  (Chan [(ECM.Canonical, String)], Chan [(ECM.Canonical, ECM.Interface)]) ->
  Chan (Localizer, [Warning], Either [Error] Result) ->
  IO ()
compileCodeService versionString moduleVersions requestChan (modReqChan, modReplyChan) compiledCode =
  forever $ do
    source <- readChan requestChan

    usedModulesResult <- pure $ EC.parseDependencies source
    (name, usedModuleNames) <- pure $ moduleRequestList moduleVersions usedModulesResult

    -- Request read of needed interfaces
    writeChan modReqChan usedModuleNames
    interfaces <- readChan modReplyChan

    context <- pure $
    -- A compiler context indicating that we need to import at least the default modules
      Context
        { _packageName = Name { user = "elm-lang", project = "test" }
        , _isExposed = False
        , _dependencies = map fst usedModuleNames
        }

    -- Make the interface map that the compiler consumes
    interfaceMap <- pure $ Map.fromList interfaces

    -- Attempt compilation
    result <- pure (EC.compile context source interfaceMap)
    writeChan compiledCode result
