module Types where

import Control.Concurrent

import Elm.Compiler as EC
import Elm.Compiler.Module as ECM
import Elm.Package
import TheMasterPlan

data CanonicalNameAndVersion = CanonicalNameAndVersion ECM.Canonical String
    deriving (Ord, Eq, Show)

type DepMap = TheMasterPlan.ProjectGraph Location
data NameAndVersion = NameAndVersion Name String
data NameAndVersionWithGraph = NameAndVersionWithGraph NameAndVersion DepMap

data StaticBuildInfo = StaticBuildInfo
    String
    [(ECM.Raw, CanonicalNameAndVersion)]
    [NameAndVersionWithGraph]

{- A container type for the communication channels we use with the elm compiler.
-}
data CommChannels = CommChannels
    (Chan String)
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

lookupModuleFromVersions ::
  StaticBuildInfo ->
  ECM.Raw ->
  [CanonicalNameAndVersion]
lookupModuleFromVersions (StaticBuildInfo versionString modVersions modGraph) rawName =
  concatMap (\(n,m) -> if n == rawName then [m] else []) modVersions
