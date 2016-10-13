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

{- Given a Canonical name from elm, produce a javascript object representing it.
Symmetric with canonicalNameFromJS
-}
canonicalNameToJS ::
  ECM.Canonical ->
  IO (JSRef a)
canonicalNameToJS (ECM.Canonical (Name user project) modPath) =
  do
    modPathArray <- JS.toJSArray (map JS.toJSString modPath)
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

{- A function that builds the relative file name in elm-stuff of an elmi file. We cheat on
the version number.
-}
fileName versionString ((ECM.Canonical (Name user project) modPath), version) =
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
convertModule :: JSRef a -> IO (ECM.Canonical, ECM.Interface)
convertModule value =
  do
    array <- JS.fromJSArray value
    name <- canonicalNameFromJS (array !! 0)
    modBody <- pure $ JS.fromJSString (array !! 1)
    interface <- base64StringToInterface modBody
    return (name, interface)

{- Given a module reply from javascript, forward a reply to the compiler with
appropriate deserialization.
-}
replyModules :: Chan [(ECM.Canonical, ECM.Interface)] -> JSRef a -> IO ()
replyModules channel value =
  do
    array <- JS.fromJSArray value
    modResults <- mapM convertModule array
    writeChan channel modResults

{- Serialize a request for a single module to be sent to the module provider on
the other side of the interop boundary.
-}
moduleRequestValue :: String -> (ECM.Canonical, String) -> IO (JSRef a)
moduleRequestValue versionString ((ECM.Canonical (Name user project) modPath), version) =
  let name = (ECM.Canonical (Name user project) modPath) in
  do
    packedName <- canonicalNameToJS name
    packedRequest <- JS.toJSArray [JS.toJSString (fileName versionString (name, version)), packedName]
    return packedRequest

{- An async loop that receives module load requests from the compiler,
forwards them through interop with a callback that sends the results back
to the elm compiler.
-}
moduleLoadService ::
  String ->
  JSRef a ->
  (Chan [(ECM.Canonical, String)], Chan [(ECM.Canonical, ECM.Interface)]) ->
  IO ()
moduleLoadService versionString loadModules (request,reply) =
  forever $ do
    interfaces <- readChan request
    moduleRequestArray <- mapM (moduleRequestValue versionString) interfaces
    moduleRequests <- JS.toJSArray moduleRequestArray
    loadedModuleCallback <- makeCallback (replyModules reply)
    jsRequestObject <- JS.toJSArray [moduleRequests, loadedModuleCallback]
    runAction loadModules jsRequestObject

{- A container type for the communication channels we use with the elm compiler.
-}
data CommChannels = CommChannels
    (Chan String)
    (Chan [(ECM.Canonical, String)])
    (Chan [(ECM.Canonical, ECM.Interface)])
    (Chan (EC.Localizer, [EC.Warning], Either [EC.Error] EC.Result))

{- Compile: Main service function used by interop.
Given a javascript value containing source code and a compile finished callback,
kick off compilation by making an appropriate request to the compiler.  It is
run asynchronously by haskell, so it can be written as a linear process.
-}
compile :: CommChannels -> JSRef a -> IO ()
compile (CommChannels compileRequestInterface requestReadInterface replyReadInterface compileReplyInterface) arg = do
  array <- JS.fromJSArray arg
  source <- pure $ array !! 0
  callback <- pure $ array !! 1

  writeChan compileRequestInterface (JS.fromJSString source)
  (localizer, warnings, result) <- readChan compileReplyInterface

  case result of
    Left e ->
      runAction
        callback
          (JS.toJSString
            (
              (List.intercalate
                "\n"
                (map (EC.errorToString localizer "" (JS.fromJSString source)) e)
              ) ++ "\n"
            )
          )
    Right (EC.Result docs interface js) ->
      runAction callback (JS.toJSString (LazyText.unpack js))

moduleVersionFromJS modVersionJS = do
  array <- JS.fromJSArray modVersionJS
  (ECM.Canonical (Name user project) name) <- canonicalNameFromJS (array !! 0)
  version <- pure $ JS.fromJSString (array !! 1)
  return (name, ((ECM.Canonical (Name user project) name), version))

moduleVersionsFromJS modVersionsJS = do
  array <- JS.fromJSArray modVersionsJS
  versions <- mapM moduleVersionFromJS array
  return versions

{- initCompiler: Entry point used by interop.  This is the only directly
callable function by interop.  Interop uses this to obtain a compiler reference.
It starts the async services used to chain the various data providers together
to serve the goal of compiling elm code.
-}
initCompiler :: JSRef a -> JSRef a -> JSRef a -> IO ()
initCompiler modVersionsJS loadModules callback =
  let (Version major minor patch) = EC.version in
  let versionString = List.intercalate "." (map show [major, minor, patch]) in
  do
    requestReadInterface <- newChan
    replyReadInterface <- newChan

    compileRequestInterface <- newChan
    compileReplyInterface <- newChan

    modVersions <- moduleVersionsFromJS modVersionsJS

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
        loadModules
        (requestReadInterface, replyReadInterface)

    compileCallback <-
      makeCallback
        (compile
          (CommChannels
            compileRequestInterface
            requestReadInterface
            replyReadInterface
            compileReplyInterface
          )
        )

    runAction callback compileCallback

#endif
