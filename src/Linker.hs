{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Linker where

import qualified Data.List as List
import           Elm.Compiler.Module as ECM

data DepMap = DepMap [(ECM.Raw, [ECM.Raw])] deriving (Show)

buildGraphInner :: DepMap -> [ECM.Raw] -> [ECM.Raw]
buildGraphInner (DepMap graphMap) result =
    -- Get a list of modules with empty dependencies at this point along with
    -- the partitioned remaining ones.
    let (canEmitPairs, notYet) = List.partition (\(name,val) -> val == []) graphMap in
    -- Just get the names of the ones we can emit
    let canEmit = map fst canEmitPairs in
    -- Compose the revised load list without any modules we've decided to emit
    -- at this point.
    let notYetRemoveEmitted = map (\(name,val) -> (name, (filter (\m -> (List.find (\e -> m == e) canEmit) /= Nothing) val))) notYet in
    if notYetRemoveEmitted == [] then
        -- We're done
        result ++ canEmit
    else
        -- Run another cycle
        buildGraphInner (DepMap notYetRemoveEmitted) (result ++ canEmit)

-- Given a list of static dependencies, along with the main module and its
-- dependencies, produce an in order list of module loads.
buildGraph :: DepMap -> ECM.Raw -> [ECM.Raw] -> [ECM.Raw]
buildGraph (DepMap graphData) topname toplev =
    buildGraphInner (DepMap (graphData ++ [(topname, toplev)])) []
