{-# OPTIONS_GHC -Wall #-}

module Compiler where

import           Control.Monad       (forever)
import           Control.Concurrent
import qualified Data.List           as List
import qualified Data.Map            as Map
import           Data.Maybe
import           Data.Function

import           Elm.Compiler as EC
import           Elm.Compiler.Module as ECM
import           Elm.Package as EP

import Types
import qualified Utils.File
import Utils.Misc

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

canonicalNameMatchingRaw ::
  StaticBuildInfo ->
  ECM.Raw ->
  [CanonicalNameAndVersion]
canonicalNameMatchingRaw (StaticBuildInfo versionString modVersions modGraph) rawName =
  modVersions
    & concatMap (\(rn, (CanonicalNameAndVersion canonical version)) -> if rn == rawName then [CanonicalNameAndVersion canonical version] else [])

singletonLookup ::
  StaticBuildInfo ->
  ECM.Raw ->
  IO [CanonicalNameAndVersion]
singletonLookup (StaticBuildInfo versionString modVersions modGraph) rawName = do
  putStrLn $ "modVersions " ++ (show modVersions)
  lookedUp <- pure $ lookup rawName modVersions
  putStrLn $ "lookedUp " ++ (show lookedUp)
  listed <- pure $ maybeToList $ lookedUp
  putStrLn $ "listed " ++ (show listed)
  return listed

moduleRequestListRight ::
  StaticBuildInfo ->
  ECM.Raw ->
  [ECM.Raw] ->
  IO (ECM.Raw, [ECM.Raw])
moduleRequestListRight sb@(StaticBuildInfo versionString modVersions modGraph) myname imports =
  do
    allNames <- pure $ List.union imports (trace importModules)
    putStrLn $ "allNames " ++ (show allNames)
    allNames2 <- mapM (singletonLookup sb) allNames
    putStrLn $ "allNames2 " ++ (show allNames2)
    result <- pure $ map rawNameFromCanonicalNameAndVersion (List.concat allNames2)
    putStrLn $ "result " ++ (show result)
    return (myname, result)

moduleRequestList ::
  StaticBuildInfo ->
  Either [Error] (Tag, ECM.Raw, [ECM.Raw]) ->
  IO (ECM.Raw, [ECM.Raw])
moduleRequestList sb@(StaticBuildInfo versionString modVersions modGraph) parseResult =
  -- Make a list of the names of modules we need to import
  case parseResult of
    Left errors -> do
      putStrLn "errors"
      return ([],[])
    Right (_, myname, imports) -> do
      putStrLn "moduleRequestList success"
      putStrLn $ "myname " ++ (show myname)
      putStrLn $ "imports " ++ (show imports)
      moduleRequestListRight sb myname imports

performCompilation ::
  [CanonicalNameAndVersion] ->
  [(CanonicalNameAndVersion, ECM.Interface)] ->
  String ->
  (Localizer, [Warning], Either [Error] CompileResult)
performCompilation usedModuleNames interfaces source =
    -- A compiler context indicating that we need to import at least the default modules
    let context = Context { _packageName = Name { user = "elm-lang", project = "test" }
        , _isExposed = False
        , _dependencies =
             map
                (\(CanonicalNameAndVersion (ECM.Canonical (Name user project) rawName) version) -> (ECM.Canonical (Name user project) rawName))
                (trace usedModuleNames)
        }
    in
    -- Make the interface map that the compiler consumes
    let interfaceMap = interfaces
            & map
                (\((CanonicalNameAndVersion canonical version), interface) ->
                    (canonical, interface))
            & Map.fromList in

    -- Attempt compilation
    let (localizer, warnings, result) = EC.compile context source interfaceMap in
    let rawNameOfModule (CanonicalNameAndVersion (ECM.Canonical (Name user project) rawName) version) = rawName in
    let resultAndDeps = case result of
         Left err -> Left err
         Right r -> Right (CompileResult r (["Main"], map rawNameOfModule usedModuleNames))
    in
    (localizer, warnings, resultAndDeps)

compileCodeService ::
  StaticBuildInfo ->
  Chan String ->
  (Chan (ECM.Raw, [ECM.Raw]), Chan [(CanonicalNameAndVersion, ECM.Interface)]) ->
  Chan (Localizer, [Warning], Either [Error] CompileResult) ->
  IO ()
compileCodeService sb@(StaticBuildInfo versionString moduleVersions modGraph) requestChan (modReqChan, modReplyChan) compiledCode =
  forever $ do
    source <- readChan requestChan

    putStrLn $ "Source " ++ source

    usedModulesResult <- pure $ EC.parseDependencies source
    (name, usedModuleNames) <- moduleRequestList sb usedModulesResult

    putStrLn $ "Got used modules" ++ (show usedModuleNames)

    -- Request read of needed interfaces
    usedCanonicalModuleNames <- pure $ trace (concatMap (lookupModuleFromVersions sb) usedModuleNames)
    writeChan modReqChan (name, map Types.rawNameFromCanonicalNameAndVersion (trace usedCanonicalModuleNames))
    interfaces <- readChan modReplyChan

    (localizer, warnings, resultAndDeps) <- pure $ performCompilation usedCanonicalModuleNames interfaces source

    writeChan compiledCode (localizer, warnings, resultAndDeps)
