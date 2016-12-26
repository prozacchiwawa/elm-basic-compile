{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

{-# OPTIONS_GHC -Wall #-}

module JSTypes where

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy.Char8 as C8S
import qualified Data.ByteString.Base64.Lazy as LB64

import qualified Elm.Compiler.Module as ECM
import Elm.Package

#ifdef __GHCJS__

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import GHCJS.Prim as JS
import qualified Unsafe.Coerce as UCK

data CanonicalNameAndVersion = CanonicalNameAndVersion ECM.Canonical String
    deriving (Ord, Eq, Show)

{- Wrap a 1-argument callback in asyncCallback1 and cast it to a consumable
type for interop.  The short answer is that I'm likely missing an imported type
class that would previously have bridged the gap between the Callback functor
and the js interop consumer.  This works for now.
-}
makeCallback :: (JSVal -> IO ()) -> IO (JSVal)
makeCallback callback =
  UCK.unsafeCoerce (asyncCallback1 callback)

rawNameToJS :: ECM.Raw -> IO (JSVal)
rawNameToJS modPath = do
  JS.toJSArray (map JS.toJSString modPath)

{- Given a Canonical name from elm, produce a javascript object representing it.
Symmetric with canonicalNameFromJS
-}
canonicalNameToJS ::
  ECM.Canonical ->
  IO (JSVal)
canonicalNameToJS (ECM.Canonical name modPath) =
  do
    modPathArray <- rawNameToJS modPath
    pnameArray <- JS.toJSArray [JS.toJSString (user name), JS.toJSString (project name)]
    obj <- JS.toJSArray [pnameArray, modPathArray]
    return obj

{- Deserialize an Elm Canonical name from a JSVal.
-}
canonicalNameFromJS ::
  JSVal ->
  IO ECM.Canonical
canonicalNameFromJS jsObj =
  do
    obj <- JS.fromJSArray jsObj
    modName <- nameFromJS (obj !! 0)
    modPathArray <- JS.fromJSArray (obj !! 1)
    return (
      ECM.Canonical
        modName
        (map JS.fromJSString modPathArray)
      )

canonicalNameAndVersionFromJS ::
  JSVal ->
  IO CanonicalNameAndVersion
canonicalNameAndVersionFromJS jsObj =
  do
    obj <- JS.fromJSArray jsObj
    canonical <- canonicalNameFromJS (obj !! 0)
    version <- pure $ JS.fromJSString (obj !! 1)
    result <- pure $ CanonicalNameAndVersion canonical version
    return result

canonicalNameAndVersionToJS ::
  CanonicalNameAndVersion ->
  IO (JSVal)
canonicalNameAndVersionToJS (CanonicalNameAndVersion canonical version) =
  do
    canonicalJS <- canonicalNameToJS canonical
    JS.toJSArray [canonicalJS, JS.toJSString version]

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

interfaceToBase64String ::
  ECM.Interface ->
  String
interfaceToBase64String intf =
  let bytestring = Binary.encode intf in
  let bits = LB64.encode bytestring in
  C8S.unpack bits

nameFromJS :: JSVal -> IO Name
nameFromJS nameArrayJS = do
  nameArray <- JS.fromJSArray nameArrayJS
  return (Name (JS.fromJSString (nameArray !! 0)) (JS.fromJSString (nameArray !! 1)))

{- Consume one javascript object from the module provider and return a pair of
Canonical name and Interface from Elm.
-}
convertModule :: JSVal -> IO (CanonicalNameAndVersion, ECM.Interface)
convertModule value = do
  array <- JS.fromJSArray value
  name <- canonicalNameAndVersionFromJS (array !! 0)
  modBody <- pure $ JS.fromJSString (array !! 1)
  interface <- base64StringToInterface modBody
  return (name, interface)

convertInterfaceToJS :: ECM.Interface -> IO JSVal
convertInterfaceToJS intf = do
  return $ JS.toJSString (interfaceToBase64String intf)

boolToJS :: Bool -> IO JSVal
boolToJS b = do
  case b of
    True -> return $ JS.toJSString "true"
    False -> return $ JS.toJSString "false"

boolFromJS :: JSVal -> IO Bool
boolFromJS js = do
  return $ JS.fromJSString js /= "false"

#endif
