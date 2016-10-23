{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

{-# OPTIONS_GHC -Wall #-}

module JSInterface where

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as LBS
import Control.Monad (forever)
import Control.Concurrent
import Data.List as List
import Data.Function
import qualified Data.Text.Lazy as LazyText
import qualified Data.ByteString.Lazy.Char8 as C8S
import qualified Data.ByteString.Base64.Lazy as LB64

import qualified Compiler as C

import qualified Elm.Compiler as EC
import qualified Elm.Compiler.Module as ECM
import Elm.Package

#ifdef __GHCJS__

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import GHCJS.Prim as JS
import qualified Unsafe.Coerce as UCK
import qualified GHC.Exts as Exts

import Linker

foreign import javascript unsafe
  "(function() { var run = $1; var arg = $2; return run(arg); })()"
  runAction :: JSRef a -> JSRef b -> IO ()

foreign import javascript unsafe
  "console.log($1)"
  trace_ :: JSRef a -> IO ()

{- Wrap a 1-argument callback in asyncCallback1 and cast it to a consumable
type for interop.  The short answer is that I'm likely missing an imported type
class that would previously have bridged the gap between the Callback functor
and the js interop consumer.  This works for now.
-}
makeCallback :: (JSRef a -> IO ()) -> IO (JSRef b)
makeCallback callback =
  UCK.unsafeCoerce (asyncCallback1 callback)

{- output a string.  Don't leave home without it. -}
trace :: String -> IO ()
trace str = trace_ (JS.toJSString str)

rawNameToJS :: ECM.Raw -> IO (JSRef a)
rawNameToJS modPath = do
  JS.toJSArray (map JS.toJSString modPath)

{- Given a Canonical name from elm, produce a javascript object representing it.
Symmetric with canonicalNameFromJS
-}
canonicalNameToJS ::
  ECM.Canonical ->
  IO (JSRef a)
canonicalNameToJS (ECM.Canonical (Name user project) modPath) =
  do
    modPathArray <- rawNameToJS modPath
    obj <- JS.toJSArray [JS.toJSString user, JS.toJSString project, modPathArray]
    return obj

{- Deserialize an Elm Canonical name from a JSref.
-}
canonicalNameFromJS ::
  JSRef a ->
  IO ECM.Canonical
canonicalNameFromJS jsObj =
  do
    obj <- JS.fromJSArray jsObj
    modPathArray <- JS.fromJSArray (obj !! 2)
    return (
      ECM.Canonical
        (Name
          (JS.fromJSString (obj !! 0))
          (JS.fromJSString (obj !! 1))
        )
        (map JS.fromJSString modPathArray)
      )

canonicalNameAndVersionFromJS ::
  JSRef a ->
  IO C.CanonicalNameAndVersion
canonicalNameAndVersionFromJS jsObj =
  do
    obj <- JS.fromJSArray jsObj
    canonical <- canonicalNameFromJS (obj !! 0)
    version <- pure $ JS.fromJSString (obj !! 1)
    result <- pure $ C.CanonicalNameAndVersion canonical version
    putStrLn ("canonicalNameAndVersionFromJS " ++ (show result))
    return result

canonicalNameAndVersionToJS ::
  C.CanonicalNameAndVersion ->
  IO (JSRef a)
canonicalNameAndVersionToJS (C.CanonicalNameAndVersion canonical version) =
  do
    canonicalJS <- canonicalNameToJS canonical
    JS.toJSArray [canonicalJS, JS.toJSString version]

{- A function that builds the relative file name in elm-stuff of an elmi file. We cheat on
the version number.
-}
fileName versionString (C.CanonicalNameAndVersion (ECM.Canonical (Name user project) modPath) version) =
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

objName versionString (C.CanonicalNameAndVersion (ECM.Canonical (Name user project) modPath) version) =
  let hyphenate rawName = List.intercalate "-" rawName in
  List.intercalate "/"
           [ "elm-stuff"
           , "build-artifacts"
           , versionString
           , user
           , project
           , version
           , (hyphenate modPath) ++ ".elmo"
           ]

{- Given a base64 string, use Interface's Binary typeclass to yield an Elm
Interface.  These are consumed by the compiler to provide signatures and values
from imported modules.  The compiler calls out to request these because the
code being compiled gets to choose which modules will be used.
-}
base64StringToInterface ::
  String ->
  IO ECM.Interface
base64StringToInterface b64 =
  let bytestring = C8S.pack b64 in
  let bits = LB64.decode bytestring in
  do
    case bits of
      Left e ->
        ioError (userError e)
      Right v ->
        case Binary.decodeOrFail v of
          Left (_, _, e) ->
            ioError (userError e)

          Right (_, _, value) ->
            return value

{- Consume one javascript object from the module provider and return a pair of
Canonical name and Interface from Elm.
-}
convertModule :: JSRef a -> IO (C.CanonicalNameAndVersion, ECM.Interface)
convertModule value =
  do
    array <- JS.fromJSArray value
    name <- canonicalNameAndVersionFromJS (array !! 0)
    modBody <- pure $ JS.fromJSString (array !! 1)
    interface <- base64StringToInterface modBody
    return (name, interface)

{- Given a module reply from javascript, forward a reply to the compiler with
appropriate deserialization.
-}
replyModules :: Chan [(C.CanonicalNameAndVersion, ECM.Interface)] -> JSRef a -> IO ()
replyModules channel value =
  do
    array <- JS.fromJSArray value
    modResults <- mapM convertModule array
    writeChan channel modResults

{- Serialize a request for a single module to be sent to the module provider on
the other side of the interop boundary.
-}
moduleRequestValue :: String -> C.CanonicalNameAndVersion -> IO (JSRef a)
moduleRequestValue versionString name =
  do
    packedName <- canonicalNameAndVersionToJS name
    packedRequest <-
      JS.toJSArray [JS.toJSString (fileName versionString name), packedName]
    return packedRequest

{- An async loop that receives module load requests from the compiler,
forwards them through interop with a callback that sends the results back
to the elm compiler.
-}
moduleLoadService ::
  String ->
  [(ECM.Raw, C.CanonicalNameAndVersion)] ->
  DepMap ->
  JSRef a ->
  (Chan (ECM.Raw, [ECM.Raw]), Chan [(C.CanonicalNameAndVersion, ECM.Interface)]) ->
  IO ()
moduleLoadService versionString modVersions (DepMap depmap) loadModules (request,reply) =
  forever $ do
    (name, usedRawNames) <- readChan request
    interfacesRaw <- pure $ Linker.buildGraph (DepMap depmap) name usedRawNames
    interfaces <- pure $ concatMap (C.canonicalNameMatchingRaw modVersions) interfacesRaw
    putStrLn ("Loading " ++ (show interfaces))
    moduleRequestArray <- mapM (moduleRequestValue versionString) interfaces
    moduleRequests <- JS.toJSArray moduleRequestArray
    loadedModuleCallback <- makeCallback (replyModules reply)
    jsRequestObject <- JS.toJSArray [moduleRequests, loadedModuleCallback]
    runAction loadModules jsRequestObject

replyObjs reply value = do
  array <- JS.fromJSArray value
  writeChan reply $ map JS.fromJSString array

headWithDefault ::
  [a] ->
  a ->
  a
headWithDefault listA a =
  case listA of
    x : _ -> x
    [] -> a

objectFileRequestValue ::
  String ->
  [(ECM.Raw, C.CanonicalNameAndVersion)] ->
  ECM.Raw ->
  IO (JSRef a)
objectFileRequestValue versionString modVersions rawName =
  do
    objFileName <- pure $
      (C.canonicalNameMatchingRaw modVersions rawName)
      & concatMap
          (\(C.CanonicalNameAndVersion (ECM.Canonical (Name user project) rawName) version) ->
            objName
              versionString
              (C.CanonicalNameAndVersion (ECM.Canonical (Name user project) rawName) version)
          )
    return $ JS.toJSString objFileName

objectFileService ::
  String ->
  [(ECM.Raw, C.CanonicalNameAndVersion)] ->
  DepMap ->
  JSRef a ->
  (Chan [ECM.Raw], Chan [String]) ->
  IO ()
objectFileService versionString modVersions (DepMap modGraph) loadObjs (request,reply) =
  forever $ do
    interfaces <- readChan request
    objRequests <- mapM (objectFileRequestValue versionString modVersions) interfaces
    objRequestsJS <- JS.toJSArray objRequests
    loadedObjCallback <- makeCallback (replyObjs reply)
    jsRequestObject <- JS.toJSArray [objRequestsJS, loadedObjCallback]
    runAction loadObjs jsRequestObject

{- A container type for the communication channels we use with the elm compiler.
-}
data CommChannels = CommChannels
    (Chan String)
    (Chan (ECM.Raw, [ECM.Raw]))
    (Chan [(C.CanonicalNameAndVersion, ECM.Interface)])
    (Chan [ECM.Raw])
    (Chan [String])
    (Chan (EC.Localizer, [EC.Warning], Either [EC.Error] C.CompileResult))

linkAndDeliver depmap requestObjInterface replyObjInterface callback js name deps =
  let emitOrderNameList = Linker.buildGraph depmap name deps in
  do
    writeChan requestObjInterface emitOrderNameList
    jsObjectData <- readChan replyObjInterface

    finishedJavascript <- pure $ jsObjectData ++ [js]
    finishedJS <- pure $ List.intercalate "\n" finishedJavascript & JS.toJSString

    runAction callback finishedJS

{- Compile: Main service function used by interop.
Given a javascript value containing source code and a compile finished callback,
kick off compilation by making an appropriate request to the compiler.  It is
run asynchronously by haskell, so it can be written as a linear process.
-}
compile :: Linker.DepMap -> CommChannels -> JSRef a -> IO ()
compile depmap (CommChannels compileRequestInterface requestReadInterface replyReadInterface requestObjInterface replyObjInterface compileReplyInterface) arg = do
  array <- JS.fromJSArray arg
  sourceJS <- pure $ array !! 0
  callback <- pure $ array !! 1

  source <- pure $ JS.fromJSString sourceJS

  putStrLn source

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
    Right (C.CompileResult (EC.Result docs interface js) (name, deps)) ->
      linkAndDeliver depmap requestObjInterface replyObjInterface callback (LazyText.unpack js) name deps

moduleVersionFromJS arr = do
  c <- canonicalNameAndVersionFromJS arr
  (C.CanonicalNameAndVersion (ECM.Canonical (Name user project) rawName) version) <- pure c
  return (rawName, c)

rawNameFromJSArray arr = do
  array <- JS.fromJSArray arr
  return $ map JS.fromJSString array

readModuleGraphRowFromJS modGraphRow = do
  array <- JS.fromJSArray modGraphRow
  name <- rawNameFromJSArray (array !! 0)
  dependsOnJS <- JS.fromJSArray (array !! 1)
  dependsOn <- mapM rawNameFromJSArray dependsOnJS
  return (name, dependsOn)

lookupModuleFromVersions modVersions rawName =
  concatMap (\(n,m) -> if n == rawName then [m] else []) modVersions

moduleVersionsFromJS :: JSRef a -> IO ([(ECM.Raw, C.CanonicalNameAndVersion)], Linker.DepMap)
moduleVersionsFromJS modVersionsJS = do
  array <- JS.fromJSArray modVersionsJS
  depsArray <- JS.fromJSArray $ array !! 0
  versions <- mapM moduleVersionFromJS depsArray
  graphArrayJS <- pure $ array !! 1
  graphArray <- JS.fromJSArray graphArrayJS
  graphRows <- mapM readModuleGraphRowFromJS graphArray

  return (versions, Linker.DepMap graphRows)

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

    trace (show modVersions)
    trace (show modGraph)

    forkIO $
      C.compileCodeService
        versionString
        modVersions
        compileRequestInterface
        (requestReadInterface, replyReadInterface)
        compileReplyInterface

    forkIO $
      moduleLoadService
        versionString
        modVersions
        modGraph
        loadModules
        (requestReadInterface, replyReadInterface)

    forkIO $
      objectFileService
        versionString
        modVersions
        modGraph
        loadJSObj
        (requestObjectFilesInterface, replyObjectFilesInterface)

    compileCallback <-
      makeCallback
        (compile
          modGraph
          (CommChannels
            compileRequestInterface
            requestReadInterface
            replyReadInterface
            requestObjectFilesInterface
            replyObjectFilesInterface
            compileReplyInterface
          )
        )

    trace "About to call callback"

    runAction callback compileCallback

#endif
