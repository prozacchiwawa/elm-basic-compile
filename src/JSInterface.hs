{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

{-# OPTIONS_GHC -Wall #-}

module JSInterface where

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

import qualified Elm.Compiler as EC
import qualified Elm.Compiler.Module as ECM

import System.IO

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

    parseRequestChan <- newChan
    parseReplyChan <- newChan

    compileRequestChan <- newChan
    compileReplyChan <- newChan

    forkIO $
      parseCodeService parseRequestChan parseReplyChan

    forkIO $
      compileInterfaceService compileRequestChan compileReplyChan

    parseCallback <-
      makeCallback (parseCode parseRequestChan parseReplyChan)

    interfaceCallback <-
      makeCallback (compileInterface compileRequestChan compileReplyChan)

    callbacks <- JS.toJSArray [parseCallback, interfaceCallback]

    runAction callback callbacks

#endif
