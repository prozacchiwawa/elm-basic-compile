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

foreign import javascript unsafe "(function() { var run = $1; var arg = $2; return run(arg); })()" runAction :: JSRef a -> JSRef b -> IO ()

foreign import javascript unsafe "console.log($1)" trace_ :: JSRef a -> IO ()

makeCallback :: (JSRef a -> IO ()) -> IO (JSRef b)
makeCallback callback =
  UCK.unsafeCoerce (asyncCallback1 callback)
    
trace :: String -> IO ()
trace str = trace_ (JS.toJSString str)

canonicalNameToJS ::
  ECM.Canonical ->
  IO (JSRef a)
canonicalNameToJS (ECM.Canonical (Name user project) modPath) =
  do
    modPathArray <- JS.toJSArray (map JS.toJSString modPath)
    obj <- JS.toJSArray [JS.toJSString user, JS.toJSString project, modPathArray]
    return obj

canonicalNameFromJS ::
  JSRef a ->
  IO ECM.Canonical
canonicalNameFromJS jsObj =
  do
    obj <- JS.fromJSArray jsObj
    modPathArray <- JS.fromJSArray (obj !! 2)
    return (ECM.Canonical (Name (JS.fromJSString (obj !! 0)) (JS.fromJSString (obj !! 1))) (map JS.fromJSString modPathArray))
    
-- A function that builds the relative file name in elm-stuff of an elmi file. We cheat on
-- the version number.
fileName versionString (ECM.Canonical (Name user project) modPath) =
  let hyphenate rawName = List.intercalate "-" rawName in
  List.intercalate "/"
           [ "elm-stuff"
           , "build-artifacts"
           , versionString
           , user
           , project
           , "4.0.0"
           , (hyphenate modPath) ++ ".elmi"
           ]

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

convertModule value =
  do
    array <- JS.fromJSArray value
    name <- canonicalNameFromJS (array !! 0)
    modBody <- pure $ JS.fromJSString (array !! 1)
    interface <- base64StringToInterface modBody
    return (name, interface)

replyModules channel value =
  do
    array <- JS.fromJSArray value
    modResults <- mapM convertModule array
    writeChan channel modResults

moduleRequestValue versionString (ECM.Canonical (Name user project) modPath) =
  let name = (ECM.Canonical (Name user project) modPath) in
  do
    packedName <- canonicalNameToJS name
    packedRequest <- JS.toJSArray [JS.toJSString (fileName versionString name), packedName]
    return packedRequest

moduleLoadService ::
  String ->
  JSRef a ->
  (Chan [ECM.Canonical], Chan [(ECM.Canonical, ECM.Interface)]) ->
  IO ()
moduleLoadService versionString loadModules (request,reply) =
  forever $ do
    interfaces <- readChan request
    moduleRequestArray <- mapM (moduleRequestValue versionString) interfaces
    moduleRequests <- JS.toJSArray moduleRequestArray
    loadedModuleCallback <- makeCallback (replyModules reply)
    jsRequestObject <- JS.toJSArray [moduleRequests, loadedModuleCallback]
    runAction loadModules jsRequestObject

data CommChannels = CommChannels
    (Chan String)
    (Chan [ECM.Canonical])
    (Chan [(ECM.Canonical, ECM.Interface)])
    (Chan (EC.Localizer, [EC.Warning], Either [EC.Error] EC.Result))

compile :: CommChannels -> JSRef a -> IO ()
compile (CommChannels compileRequestInterface requestReadInterface replyReadInterface compileReplyInterface) arg = do
  array <- JS.fromJSArray arg
  source <- pure $ array !! 0
  callback <- pure $ array !! 1
  
  writeChan compileRequestInterface (JS.fromJSString source)
  (localizer, warnings, result) <- readChan compileReplyInterface

  case result of
    Left e ->
      runAction callback (JS.toJSString ((List.intercalate "\n" (map (EC.errorToString localizer "" (JS.fromJSString source)) e)) ++ "\n"))
    Right (EC.Result docs interface js) ->
      runAction callback (JS.toJSString (LazyText.unpack js))

initCompiler :: JSRef a -> JSRef a -> IO ()
initCompiler loadModules callback =
  let (Version major minor patch) = EC.version in
  let versionString = List.intercalate "." (map show [major, minor, patch]) in
  do
    requestReadInterface <- newChan
    replyReadInterface <- newChan

    compileRequestInterface <- newChan
    compileReplyInterface <- newChan

    forkIO $ C.compileCodeService versionString compileRequestInterface (requestReadInterface, replyReadInterface) compileReplyInterface

    forkIO $ moduleLoadService versionString loadModules (requestReadInterface, replyReadInterface)

    compileCallback <- makeCallback (compile (CommChannels compileRequestInterface requestReadInterface replyReadInterface compileReplyInterface))

    runAction callback compileCallback

#endif
