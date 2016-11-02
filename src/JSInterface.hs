{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

{-# OPTIONS_GHC -Wall #-}

module JSInterface where

import Types
import JSTypes
import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import Control.Monad (forever)
import Control.Concurrent
import Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Function
import qualified Data.Text.Lazy as LazyText

import qualified Compiler as C

import qualified Elm.Compiler as EC
import qualified Elm.Compiler.Module as ECM

import Types
import Utils.Misc

import Elm.Package
import Elm.Package.Solution

import TheMasterPlan
import BuildManager as BM

#ifdef __GHCJS__

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import GHCJS.Prim as JS
import qualified Unsafe.Coerce as UCK
import qualified GHC.Exts as Exts

foreign import javascript unsafe
  "(function() { var run = $1; var arg = $2; return run(arg); })()"
  runAction :: JSRef a -> JSRef b -> IO ()

foreign import javascript unsafe
  "console.log($1)"
  trace_ :: JSRef a -> IO ()

{- A function that builds the relative file name in elm-stuff of an elmi file. We cheat on
the version number.
-}
fileName versionString (CanonicalNameAndVersion (ECM.Canonical (Name user project) modPath) version) =
  let hyphenate rawName = List.intercalate "-" rawName in
  List.intercalate "/"
           [ "elm-stuff"
           , "build-artifacts"
           , versionString
           , user
           , project
           , version
           , (hyphenate modPath) ++ ".elmi"
           ]

objName versionString (CanonicalNameAndVersion (ECM.Canonical (Name user project) modPath) version) =
  let hyphenate rawName = List.intercalate "-" rawName in
  case modPath of
    "Native" : rawName ->
      List.intercalate "/"
        [ "elm-stuff"
        , "packages"
        , user
        , project
        , version
        , "src"
        , "Native"
        , (hyphenate rawName) ++ ".js"
        ]
    rawName ->
      List.intercalate "/"
        [ "elm-stuff"
        , "build-artifacts"
        , versionString
        , user
        , project
        , version
        , (hyphenate modPath) ++ ".elmo"
        ]

{- Given a module reply from javascript, forward a reply to the compiler with
appropriate deserialization.
-}
replyModules :: Chan [(CanonicalNameAndVersion, ECM.Interface)] -> JSRef a -> IO ()
replyModules channel value =
  do
    array <- JS.fromJSArray value
    modResults <- mapM convertModule array
    writeChan channel modResults

{- Serialize a request for a single module to be sent to the module provider on
the other side of the interop boundary.
-}
moduleRequestValue :: String -> CanonicalNameAndVersion -> IO (JSRef a)
moduleRequestValue versionString name =
  do
    packedName <- canonicalNameAndVersionToJS name
    packedRequest <-
      JS.toJSArray [JS.toJSString (fileName versionString name), packedName]
    return packedRequest

moduleCanonicalNameAndVersionListForRawName ::
  ECM.Raw ->
  [(ECM.Raw, CanonicalNameAndVersion)] ->
  [CanonicalNameAndVersion]
moduleCanonicalNameAndVersionListForRawName raw rcnvlist =
  rcnvlist & filter (\(r, cnv) -> raw == r) & map snd

setOfNameAndVersionFromCanonicalNameAndVersionList ::
  [CanonicalNameAndVersion] ->
  Set.Set NameAndVersion
setOfNameAndVersionFromCanonicalNameAndVersionList cnvlist =
  foldl' (\s (CanonicalNameAndVersion (ECM.Canonical n r) v) -> Set.insert (NameAndVersion n v) s) Set.empty cnvlist

depMapsMatchingNameAndVersionSet ::
  [NameAndVersionWithGraph] ->
  Set.Set NameAndVersion ->
  IO [DepMap]
depMapsMatchingNameAndVersionSet depmap nvSet = do
  result <- pure $ depmap
    & filter (\(NameAndVersionWithGraph nv g) -> Set.member nv nvSet)
    & map (\(NameAndVersionWithGraph nv g) -> g)
  return result

lookupGraphFromRawName :: StaticBuildInfo -> ECM.Raw -> IO [DepMap]
lookupGraphFromRawName sb@(StaticBuildInfo versionString modVersions depmap) raw =
  let canonicalNameAndVersionListForRawName = moduleCanonicalNameAndVersionListForRawName raw modVersions in
  do
    nameAndVersionSet <- pure $ setOfNameAndVersionFromCanonicalNameAndVersionList canonicalNameAndVersionListForRawName
    depMapsMatchingNameAndVersionSet depmap nameAndVersionSet
    
canonicalModulesFromDepMap ::
  DepMap ->
  [CanonicalModule]
canonicalModulesFromDepMap g =
  g & projectData & Map.toList & map snd & concatMap projectDependencies

getRawNamesFromDepMap ::
  DepMap ->
  [ECM.Raw]
getRawNamesFromDepMap g =
  let canonicalModules = canonicalModulesFromDepMap g in
  map (\cn -> name cn) canonicalModules

getRawDepsFromRawName ::
  StaticBuildInfo ->
  ECM.Raw ->
  IO [ECM.Raw]
getRawDepsFromRawName sb@(StaticBuildInfo versionString modVersions depmap) raw = do
  lookedUp <- lookupGraphFromRawName sb raw
  depsFromLookedUp <- pure $ map getRawNamesFromDepMap lookedUp
  putStrLn $ "depsFromLookedUp " ++ (show raw) ++ " -> " ++ (show depsFromLookedUp)
  return $ concat depsFromLookedUp & filter (\m -> m /= raw)

buildOutInterfaceListInner ::
  StaticBuildInfo ->
  [ECM.Raw] ->
  [(ECM.Raw, [ECM.Raw])] ->
  IO [ECM.Raw]
buildOutInterfaceListInner sb@(StaticBuildInfo versionString modVersions depmap) completed desired =
  case desired of
    [] -> do
      return completed
    (i,[]) : is -> do
      buildOutInterfaceListInner sb (i : completed) is
    (i,(d : ds)) : is -> do
      allnames <- pure $ completed ++ (map fst desired) ++ [d]
      filteredHave <- pure $ map (\(n,deps) -> (n,filter (\dd -> dd /= d) deps)) desired
      rawDepsFromRawName <- getRawDepsFromRawName sb d
      filteredDeps <- pure $ filter (\n -> not $ List.any (\(x,_) -> x /= n) filteredHave) rawDepsFromRawName
      newDesired <- pure $ ((d,filteredDeps) : filteredHave)
      putStrLn $ "buildOut " ++ (show completed) ++ " " ++ (show newDesired)
      buildOutInterfaceListInner sb completed newDesired

buildOutInterfaceList ::
  StaticBuildInfo ->
  [ECM.Raw] ->
  IO [ECM.Raw]
buildOutInterfaceList sb@(StaticBuildInfo versionString modVersions depmap) desired =
  case desired of
    [] -> do
      return []
    l -> do
      immediateDeps <- mapM (getRawDepsFromRawName sb) l
      namesAndDeps <- pure $ zip l immediateDeps
      buildOutInterfaceListInner sb [] namesAndDeps

{- An async loop that receives module load requests from the compiler,
forwards them through interop with a callback that sends the results back
to the elm compiler.
-}
moduleLoadService ::
  StaticBuildInfo ->
  JSRef a ->
  (Chan (ECM.Raw, [ECM.Raw]), Chan [(CanonicalNameAndVersion, ECM.Interface)]) ->
  IO ()
moduleLoadService sb@(StaticBuildInfo versionString modVersions depmap) loadModules (request,reply) =
  forever $ do
    (name, usedRawNames) <- readChan request
    interfacesRaw <- buildOutInterfaceList sb usedRawNames
    interfaces <- pure $ concatMap (C.canonicalNameMatchingRaw sb) interfacesRaw
    moduleRequestArray <- mapM (moduleRequestValue versionString) interfaces
    moduleRequests <- JS.toJSArray moduleRequestArray
    loadedModuleCallback <- makeCallback (replyModules reply)
    jsRequestObject <- JS.toJSArray [moduleRequests, loadedModuleCallback]
    runAction loadModules jsRequestObject

replyObjs reply value = do
  array <- JS.fromJSArray value
  writeChan reply $ map JS.fromJSString array

objectFileRequestValue ::
  StaticBuildInfo ->
  ECM.Raw ->
  IO (JSRef a)
objectFileRequestValue sb@(StaticBuildInfo versionString modVersions modGraph) rawName =
  do
    canonicalNames <- pure $ lookupModuleFromVersions sb rawName
    putStrLn $ "/* " ++ (show canonicalNames) ++ " */"
    objFileName <- pure $ canonicalNames
      & concatMap
          (\(CanonicalNameAndVersion (ECM.Canonical (Name user project) rawName) version) ->
            objName
              versionString
              (CanonicalNameAndVersion (ECM.Canonical (Name user project) rawName) version)
          )
    return $ JS.toJSString objFileName

objectFileService ::
  StaticBuildInfo ->
  JSRef a ->
  (Chan [ECM.Raw], Chan [String]) ->
  IO ()
objectFileService sb@(StaticBuildInfo versionString modVersions modGraph) loadObjs (request,reply) =
  forever $ do
    interfaces <- readChan request
    objRequests <- mapM (objectFileRequestValue sb) interfaces
    objRequestsJS <- JS.toJSArray objRequests
    loadedObjCallback <- makeCallback (replyObjs reply)
    jsRequestObject <- JS.toJSArray [objRequestsJS, loadedObjCallback]
    runAction loadObjs jsRequestObject

linkAndDeliver ::
  StaticBuildInfo ->
  Chan [ECM.Raw] ->
  Chan [String] ->
  JSRef a ->
  String ->
  ECM.Raw ->
  [ECM.Raw] ->
  IO ()
linkAndDeliver sb@(StaticBuildInfo versionString modVersions depmap) requestObjInterface replyObjInterface callback js name deps =
  do
    emitOrderNameList <- buildOutInterfaceList sb deps
    writeChan requestObjInterface (map rawNameFromCanonicalNameAndVersion (concatMap (lookupModuleFromVersions sb) emitOrderNameList))
    jsObjectData <- readChan replyObjInterface

    finishedJavascript <- pure $ (List.intercalate "\n" jsObjectData) ++ "\n" ++ js
    finishedJS <- pure $ JS.toJSString finishedJavascript

    runAction callback finishedJS

{- Compile: Main service function used by interop.
Given a javascript value containing source code and a compile finished callback,
kick off compilation by making an appropriate request to the compiler.  It is
run asynchronously by haskell, so it can be written as a linear process.
-}
compile ::
  StaticBuildInfo ->
  CommChannels ->
  JSRef a ->
  IO ()
compile sb@(StaticBuildInfo versionString modVersions depmap) (CommChannels compileRequestInterface requestReadInterface replyReadInterface requestObjInterface replyObjInterface compileReplyInterface) arg = do
  array <- JS.fromJSArray arg
  sourceJS <- pure $ array !! 0
  callback <- pure $ array !! 1

  source <- pure $ JS.fromJSString sourceJS

  writeChan compileRequestInterface source
  (localizer, warnings, result) <- readChan compileReplyInterface

  case result of
    Left e ->
      runAction
        callback
          (JS.toJSString
            (
              (List.intercalate
                "\n"
                (map (EC.errorToString localizer "" source) e)
              ) ++ "\n"
            )
          )
    Right (CompileResult (EC.Result docs interface js) (name, deps)) ->
      linkAndDeliver sb requestObjInterface replyObjInterface callback (LazyText.unpack js) name deps

moduleVersionFromJS arr = do
  c <- canonicalNameAndVersionFromJS arr
  (CanonicalNameAndVersion (ECM.Canonical (Name user project) rawName) version) <- pure c
  return (rawName, c)

rawNameFromJSArray arr = do
  array <- JS.fromJSArray arr
  return $ map JS.fromJSString array

{- initCompiler: Entry point used by interop.  This is the only directly
callable function by interop.  Interop uses this to obtain a compiler reference.
It starts the async services used to chain the various data providers together
to serve the goal of compiling elm code.
-}
initCompiler :: JSRef a -> JSRef a -> JSRef a -> IO ()
initCompiler modVersionsJS load callback =
  let (Version major minor patch) = EC.version in
  let versionString = List.intercalate "." (map show [major, minor, patch]) in
  do
    putStrLn "starting compiler"

    loadArray <- JS.fromJSArray load
    loadJSObj <- pure $ loadArray !! 0
    loadModules <- pure $ loadArray !! 1

    requestReadInterface <- newChan
    replyReadInterface <- newChan

    compileRequestInterface <- newChan
    compileReplyInterface <- newChan

    requestObjectFilesInterface <- newChan
    replyObjectFilesInterface <- newChan

    (modVersions, modGraph) <- moduleVersionsFromJS modVersionsJS

    staticBuildInfo <- pure $ StaticBuildInfo versionString modVersions modGraph

    forkIO $
      C.compileCodeService
        staticBuildInfo
        compileRequestInterface
        (requestReadInterface, replyReadInterface)
        compileReplyInterface

    forkIO $
      moduleLoadService
        staticBuildInfo
        loadModules
        (requestReadInterface, replyReadInterface)

    forkIO $
      objectFileService
        staticBuildInfo
        loadJSObj
        (requestObjectFilesInterface, replyObjectFilesInterface)

    compileCallback <-
      makeCallback
        (compile
          staticBuildInfo
          (CommChannels
            compileRequestInterface
            requestReadInterface
            replyReadInterface
            requestObjectFilesInterface
            replyObjectFilesInterface
            compileReplyInterface
          )
        )

    putStrLn "returning control"

    runAction callback compileCallback

#endif
