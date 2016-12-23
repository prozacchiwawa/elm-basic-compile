module Types where

import Control.Concurrent
import Data.Map as Map

import Elm.Compiler as EC
import Elm.Compiler.Module as ECM
import Elm.Package as EP

data CanonicalNameAndVersion = CanonicalNameAndVersion ECM.Canonical String
    deriving (Ord, Eq, Show)

data Package = Package EP.Name EP.Version deriving (Ord, Eq)

data CanonicalModule = CanonicalModule
  { package :: Types.Package
  , name :: ECM.Raw
  } deriving (Ord, Eq)

data ProjectGraph a = ProjectGraph
  { projectData :: Map.Map CanonicalModule (ProjectData a)
  , projectNatives :: Map.Map CanonicalModule Location
  }

data ProjectData a = ProjectData
  { projectLocation :: a
  , projectDependencies :: [CanonicalModule]
  }

data Location = Location
  { _relativePath :: FilePath
  , _package :: Types.Package
  }

type DepMap = ProjectGraph Location
data NameAndVersion = NameAndVersion Name String deriving (Ord, Eq, Show)
data NameAndVersionWithGraph = NameAndVersionWithGraph NameAndVersion DepMap

data StaticBuildInfo = StaticBuildInfo
    NameAndVersion
    [(ECM.Raw, CanonicalNameAndVersion)]
    [NameAndVersionWithGraph]

{- A container type for the communication channels we use with the elm compiler.
-}
data CommChannels = CommChannels
    (Chan (EP.Name,String))
    (Chan (ECM.Raw, [ECM.Raw]))
    (Chan [(CanonicalNameAndVersion, ECM.Interface)])
    (Chan [ECM.Raw])
    (Chan [String])
    (Chan (EC.Localizer, [EC.Warning], Either [EC.Error] CompileResult))

data CompileResult = CompileResult Result (ECM.Raw,[ECM.Raw])

rawNameFromCanonicalNameAndVersion ::
  CanonicalNameAndVersion ->
  ECM.Raw
rawNameFromCanonicalNameAndVersion (CanonicalNameAndVersion (ECM.Canonical (Name user project) modPath) version) =
  modPath

lookupCanonicalDepNamesFromGraph ::
  StaticBuildInfo ->
  CanonicalNameAndVersion ->
  [(CanonicalNameAndVersion, DepMap)]
lookupCanonicalDepNamesFromGraph sg@(StaticBuildInfo versionString modVersions modGraph) (CanonicalNameAndVersion (ECM.Canonical (Name user project) modPath) version) =
  concatMap (\ nag@(NameAndVersionWithGraph (NameAndVersion (Name u1 p1) version) depmap) -> if u1 == user && p1 == project then [((CanonicalNameAndVersion (ECM.Canonical (Name u1 p1) modPath) version), depmap)] else []) modGraph

lookupModuleFromVersions ::
  StaticBuildInfo ->
  ECM.Raw ->
  [CanonicalNameAndVersion]
lookupModuleFromVersions (StaticBuildInfo versionString modVersions modGraph) rawName =
  let lookupResult frn = concatMap
                         (\ (n,CanonicalNameAndVersion (ECM.Canonical name rn) v) ->
                            if n == frn then
                              [CanonicalNameAndVersion (ECM.Canonical name rawName) v]
                            else
                              []
                         ) modVersions
  in
    if any (\x -> x == rawName) [["Native","Scheduler"],["Native","Utils"],["Native","Json"]] then
      {- module that has only a native, not an elm input -}
      let res = lookupResult ["Basics"] in
        Prelude.map
        (\(CanonicalNameAndVersion (ECM.Canonical (Name u p) _) v) ->
            (CanonicalNameAndVersion (ECM.Canonical (Name u p) rawName) v)
        )
        res
    else
      case rawName of
        "Native" : tl -> lookupResult tl
        l -> lookupResult l
