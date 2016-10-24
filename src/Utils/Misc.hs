module Utils.Misc where

import Data.List

headWithDefault ::
  [a] ->
  a ->
  a
headWithDefault listA a =
  case listA of
    x : _ -> x
    [] -> a
