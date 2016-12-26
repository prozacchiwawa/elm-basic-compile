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
import Control.Monad (forever, liftM)
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

import qualified Elm.Package as EP
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

makeErrorListJS :: EC.Localizer -> [EC.Error] -> String -> IO (JSRef a)
makeErrorListJS localizer errors source = do
  hsArray <- pure $ errors
    & map (EC.errorToString EC.dummyLocalizer "" source)
    & map JS.toJSString
  JS.toJSArray hsArray

{- A function that builds the relative file name in elm-stuff of an elmi file. We cheat on
the version number.
-}
fileName versionString (CanonicalNameAndVersion (ECM.Canonical (EP.Name user project) modPath) version) =
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

objName versionString (CanonicalNameAndVersion (ECM.Canonical (EP.Name user project) modPath) version) =
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

  depOrderNames <- pure $ buildOutInterfaceList (objGetRawDepsFromRawName deps) (map fst deps)

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
    objFileName <- pure $ canonicalNames
      & concatMap
          (\n@(CanonicalNameAndVersion (ECM.Canonical (EP.Name user project) rawName) version) ->
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
    Left e -> do
      errorResult <- makeErrorListJS localizer e source
      runAction callback errorResult
    Right (CompileResult (EC.Result docs interface js) (name, deps)) ->
      linkAndDeliver sb requestObjInterface replyObjInterface callback (LazyText.unpack js) name deps

doParseCode name source =
  let usedModulesResult = EC.parseDependencies name source in
  case usedModulesResult of
    Left errors ->
      (errors, [], [])
    Right (_, myname, imports) ->
      ([], myname, imports)

makeParseReply :: ECM.Raw -> [ECM.Raw] -> IO (JSRef a)
makeParseReply myname imports = do
  nameJS <- rawNameToJS myname
  importsWithJS <- mapM rawNameToJS imports
  importsJS <- JS.toJSArray importsWithJS
  JS.toJSArray [nameJS, importsJS]

parseCode :: Chan (EP.Name, String) -> Chan ([EC.Error], ECM.Raw, [ECM.Raw]) -> JSRef a -> IO ()
parseCode requestChan replyChan arg = do
  array <- JS.fromJSArray arg
  nameJS <- pure $ array !! 0
  sourceJS <- pure $ array !! 1
  callback <- pure $ array !! 2

  name <- nameFromJS nameJS
  source <- pure $ JS.fromJSString sourceJS
  writeChan requestChan (name, source)
  (errors, myname, imports) <- readChan replyChan

  result <- case errors of
              [] -> makeParseReply myname imports
              e -> makeErrorListJS EC.dummyLocalizer e source

  runAction callback result

parseCodeService requestChan replyChan =
  forever $ do
    (name, source) <- readChan requestChan
    compileResult <- pure $ doParseCode name source
    writeChan replyChan compileResult

encodeCorrectCompileResult :: ECM.Interface -> String -> IO (JSRef a)
encodeCorrectCompileResult interface js = do
  jstrue <- boolToJS True
  JS.toJSArray [jstrue, JS.toJSString (interfaceToBase64String interface), JS.toJSString js]

encodeIncorrectCompileResult :: EC.Localizer -> [EC.Error] -> String -> IO (JSRef a)
encodeIncorrectCompileResult localizer errors source = do
  jsfalse <- boolToJS False
  errlistJS <- makeErrorListJS localizer errors source
  JS.toJSArray [jsfalse, errlistJS]

encodeCompilerResult :: String -> (EC.Localizer, [EC.Warning], Either [EC.Error] EC.Result) -> IO (JSRef a)
encodeCompilerResult source (localization, warnings, res) =
  case res of
    Right (EC.Result maybeDocs interface js) -> encodeCorrectCompileResult interface (LazyText.unpack js)
    Left errors -> encodeIncorrectCompileResult localization errors source

compileInterface :: Chan (EC.Context, String, [(CanonicalNameAndVersion, ECM.Interface)]) -> Chan (EC.Localizer, [EC.Warning], Either [EC.Error] EC.Result) -> JSRef a -> IO ()
compileInterface requestChan replyChan arg = do
  array <- JS.fromJSArray arg
  packageName <- nameFromJS (array !! 0)
  isExposed <- boolFromJS (array !! 1)
  source <- pure $ JS.fromJSString (array !! 2)
  ifacesJS <- JS.fromJSArray (array !! 3)
  callback <- pure $ array !! 4
  ifaces <- mapM convertModule ifacesJS
  deps <- pure $ map (\(CanonicalNameAndVersion cname ver, _) -> cname) ifaces
  writeChan requestChan (EC.Context packageName isExposed deps, source, ifaces)
  putStrLn $ "waiting"
  result <- readChan replyChan
  putStrLn $ "result"
  res <- encodeCompilerResult source result
  
  putStrLn $ "callback"
  runAction callback res

compileInterfaceService :: Chan (EC.Context, String, [(CanonicalNameAndVersion, ECM.Interface)]) -> Chan (EC.Localizer, [EC.Warning], Either [EC.Error] EC.Result) -> IO ()
compileInterfaceService requestChan replyChan =
  forever $ do
    (ctx, source, ifacesCNV) <- readChan requestChan
    ifaces <- pure $ map (\(CanonicalNameAndVersion cnv ver, intf) -> (cnv,intf)) ifacesCNV
    res <- pure $ EC.compile ctx source (Map.fromList ifaces)
    writeChan replyChan res

{- initCompiler: Entry point used by interop.
callable function by interop.  Interop uses this to obtain a compiler reference.
It starts the async services used to chain the various data providers together
to serve the goal of compiling elm code.
-}
initCompiler :: JSRef a -> JSRef a -> JSRef a -> IO ()
initCompiler modVersionsJS load callback =
  let (EP.Version major minor patch) = EC.version in
  let name = EP.Name { EP.user = "elm-lang", EP.project = "test" } in
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

    parseRequestChan <- newChan
    parseReplyChan <- newChan

    compileRequestChan <- newChan
    compileReplyChan <- newChan

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

    forkIO $
      parseCodeService parseRequestChan parseReplyChan

    forkIO $
      compileInterfaceService compileRequestChan compileReplyChan

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

    parseCallback <-
      makeCallback (parseCode parseRequestChan parseReplyChan)

    interfaceCallback <-
      makeCallback (compileInterface compileRequestChan compileReplyChan)

    callbacks <- JS.toJSArray [parseCallback, interfaceCallback, compileCallback]

    runAction callback callbacks

#endif
