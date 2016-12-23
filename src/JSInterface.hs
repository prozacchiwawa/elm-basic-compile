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
import Data.String.Utils as DSU
import qualified Data.Text.Lazy as LazyText

import qualified Compiler as C

import qualified Elm.Compiler as EC
import qualified Elm.Compiler.Module as ECM

import System.IO

import Types
import Utils.Misc

import Elm.Package
import Elm.Package.Solution

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
  [DepMap]
depMapsMatchingNameAndVersionSet depmap nvSet =
  depmap
    & filter (\(NameAndVersionWithGraph nv g) -> Set.member nv nvSet)
    & map (\(NameAndVersionWithGraph nv g) -> g)

lookupGraphFromRawName :: StaticBuildInfo -> ECM.Raw -> [DepMap]
lookupGraphFromRawName sb@(StaticBuildInfo versionString modVersions depmap) raw =
  let canonicalNameAndVersionListForRawName = moduleCanonicalNameAndVersionListForRawName raw modVersions in
  let nameAndVersionSet = setOfNameAndVersionFromCanonicalNameAndVersionList canonicalNameAndVersionListForRawName in
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
  [ECM.Raw]
getRawDepsFromRawName sb@(StaticBuildInfo versionString modVersions depmap) raw =
  let lookedUp = lookupGraphFromRawName sb raw in
  let depsFromLookedUp = map getRawNamesFromDepMap lookedUp in
  concat depsFromLookedUp & filter (\m -> m /= raw)

objGetRawDepsFromRawName ::
  [(ECM.Raw, [ECM.Raw])] ->
  ECM.Raw ->
  [ECM.Raw]
objGetRawDepsFromRawName deps name =
  deps
  & filter (\(x,_) -> x == name)
  & concatMap snd

buildOutInterfaceListInner ::
  (ECM.Raw -> [ECM.Raw]) ->
  [ECM.Raw] ->
  [(ECM.Raw, [ECM.Raw])] ->
  [ECM.Raw]
buildOutInterfaceListInner getRawDeps completed desired =
  case desired of
    [] ->
      List.reverse completed
    (i,[]) : is ->
      if any (\x -> x == i) completed then
        buildOutInterfaceListInner getRawDeps completed is
      else
        buildOutInterfaceListInner getRawDeps (i : completed) is
    (i,(d : ds)) : is ->
      let rawDepsFromRawName = getRawDeps d in
      let (matched, unmatched) = partition (\(x,deps) -> x == d) is in
      let newTail = if any (\x -> x == d) ((map fst desired) ++ completed) then matched ++ [(i,ds)] ++ unmatched else (d,rawDepsFromRawName) : ((i,ds) : is) in
      buildOutInterfaceListInner getRawDeps completed newTail

buildOutInterfaceList ::
  (ECM.Raw -> [ECM.Raw]) ->
  [ECM.Raw] ->
  [ECM.Raw]
buildOutInterfaceList getRawDeps desired =
  case desired of
    [] -> []
    l ->
      let immediateDeps = map getRawDeps l in
      let namesAndDeps = zip l immediateDeps in
      buildOutInterfaceListInner getRawDeps [] namesAndDeps

{- An async loop that receives module load requests from the compiler,
forwards them through interop with a callback that sends the results back
to the elm compiler.
-}
moduleLoadService ::
  StaticBuildInfo ->
  JSRef a ->
  (Chan (ECM.Raw, [ECM.Raw]), Chan [(CanonicalNameAndVersion, ECM.Interface)]) ->
  IO ()
moduleLoadService sb@(StaticBuildInfo (NameAndVersion name versionString) modVersions depmap) loadModules (request,reply) =
  forever $ do
    (name, usedRawNames) <- readChan request
    interfacesRaw <- pure $ buildOutInterfaceList (getRawDepsFromRawName sb) usedRawNames
    interfaces <- pure $ concatMap (C.canonicalNameMatchingRaw sb) interfacesRaw
    moduleRequestArray <- mapM (moduleRequestValue versionString) interfaces
    moduleRequests <- JS.toJSArray moduleRequestArray
    loadedModuleCallback <- makeCallback (replyModules reply)
    jsRequestObject <- JS.toJSArray [moduleRequests, loadedModuleCallback]
    runAction loadModules jsRequestObject

extractJSDeps (name,jstext) =
  if DSU.startswith "//import" jstext then
    let lines = DSU.split "\n" jstext in
    case lines of
      fl : tl ->
        if DSU.endswith "//" fl then
          let strlen = List.length fl in
          let substr = List.take (strlen - 12) (List.drop 9 fl) in
          let modules =
                map (DSU.split ".") (map DSU.strip (DSU.split "," substr))
          in
          (name, modules)
        else
          (name, [])
      [] -> (name, [])
  else
    (name, [])
    
replyObj replyEnt = do
  rawNameJS <- JS.fromJSArray (replyEnt !! 0)
  textJS <- pure $ replyEnt !! 1
  rawName <- pure $ map JS.fromJSString rawNameJS
  return (rawName, JS.fromJSString textJS)

selectFromPair item pairs =
  filter (\(x,_) -> item == x) pairs

replyObjs sb@(StaticBuildInfo versionString modVersions depmap) reply value =
  do
  arrayNameJSAndTextJS <- JS.fromJSArray value
  arrayNameJSAndText <- mapM JS.fromJSArray arrayNameJSAndTextJS
  -- Elements are [rawNameJS, text]
  arrayNameAndText <- mapM replyObj arrayNameJSAndText

  -- We have the labeled object files
  -- A wrinkle here is that javascript files have an //import comment
  -- at the top which specifies their order.
  -- We'll extract those into a version of our dep list and reorder them

  deps <- pure $ map extractJSDeps arrayNameAndText

  putStrLn $ "/* deps " ++ (show deps) ++ " */"

  depOrderNames <- pure $ buildOutInterfaceList (objGetRawDepsFromRawName deps) (map fst deps)

  putStrLn $ "/* depOrderNames " ++ (show depOrderNames) ++ " */"

  reordered <- pure $ concatMap (\x -> selectFromPair x arrayNameAndText) depOrderNames

  hFlush stdout

  writeChan reply $ reordered & map snd

rawNameAndFileName ::
  (ECM.Raw, String) ->
  IO (JSRef a)
rawNameAndFileName (rawName,objFileName) = do
  rawNameJS <- rawNameToJS rawName
  objJS <- pure $ JS.toJSString objFileName
  arr <- pure $ [rawNameJS, objJS]
  JS.toJSArray arr

objectFileRequestValue ::
  StaticBuildInfo ->
  ECM.Raw ->
  IO [JSRef a]
objectFileRequestValue sb@(StaticBuildInfo (NameAndVersion name versionString) modVersions modGraph) rawName =
  do
    canonicalNames <- pure $ lookupModuleFromVersions sb rawName
    putStrLn $ "/* " ++ (show canonicalNames) ++ " */"
    objFileName <- pure $ canonicalNames
      & concatMap
          (\n@(CanonicalNameAndVersion (ECM.Canonical (Name user project) rawName) version) ->
             [(rawName,
               objName
                versionString
                n
              )
             ]
          )
    res <- mapM rawNameAndFileName objFileName
    return res

objectFileService ::
  StaticBuildInfo ->
  JSRef a ->
  (Chan [ECM.Raw], Chan [String]) ->
  IO ()
objectFileService sb@(StaticBuildInfo versionString modVersions modGraph) loadObjs (request,reply) =
  forever $ do
    interfaces <- readChan request
    objRequests <- mapM (objectFileRequestValue sb) interfaces
    objRequestsJS <- JS.toJSArray (concat objRequests)
    loadedObjCallback <- makeCallback (replyObjs sb reply)
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
    emitOrderNameList <- pure $ buildOutInterfaceList (getRawDepsFromRawName sb) deps
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
compile sb@(StaticBuildInfo (NameAndVersion name versionString) modVersions depmap) (CommChannels compileRequestInterface requestReadInterface replyReadInterface requestObjInterface replyObjInterface compileReplyInterface) arg = do
  array <- JS.fromJSArray arg
  sourceJS <- pure $ array !! 0
  callback <- pure $ array !! 1

  source <- pure $ JS.fromJSString sourceJS

  writeChan compileRequestInterface (name, source)
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
  let name = Name { user = "elm-lang", project = "test" } in
  let versionString = List.intercalate "." (map show [major, minor, patch]) in
  do
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

    staticBuildInfo <- pure $ StaticBuildInfo (NameAndVersion name versionString) modVersions modGraph

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

    runAction callback compileCallback

#endif
