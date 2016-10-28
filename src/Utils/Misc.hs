{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Utils.Misc where

import Data.List

#ifdef __GHCJS__
import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import GHCJS.Prim as JS
import qualified Unsafe.Coerce as UCK
import qualified GHC.Exts as Exts
#endif

headWithDefault ::
  [a] ->
  a ->
  a
headWithDefault listA a =
  case listA of
    x : _ -> x
    [] -> a

foreign import javascript unsafe
  "console.log($1)"
  trace_ :: JSRef a -> ()

foreign import javascript unsafe
  "console.log($1)"
  log_ :: JSRef a -> ()

log :: a -> a
log v = snd (log_ (UCK.unsafeCoerce v), v)

{- output a string.  Don't leave home without it. -}
trace :: (Show a) => a -> a
trace a = snd (trace_ (JS.toJSString (show a)), a)
