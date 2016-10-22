{-# OPTIONS_GHC -Wall #-}

module Main where

import Compiler as C
import JSInterface as JSI

import           Control.Concurrent
import qualified Data.List           as List
import qualified Data.Text.Lazy

import           Elm.Compiler as EC
import           Elm.Compiler.Module as ECM
import           Elm.Package

moduleVersions =
  [
    (["Basics"], (C.CanonicalNameAndVersion (ECM.Canonical (Name "elm-lang" "core") ["Basics"]) "4.0.0")),
    (["Debug"], (C.CanonicalNameAndVersion (ECM.Canonical (Name "elm-lang" "core") ["Debug"]) "4.0.0")),
    (["List"], (C.CanonicalNameAndVersion (ECM.Canonical (Name "elm-lang" "core") ["List"]) "4.0.0")),
    (["Maybe"], (C.CanonicalNameAndVersion (ECM.Canonical (Name "elm-lang" "core") ["Maybe"]) "4.0.0")),
    (["Result"], (C.CanonicalNameAndVersion (ECM.Canonical (Name "elm-lang" "core") ["Result"]) "4.0.0")),
    (["Platform"], (C.CanonicalNameAndVersion (ECM.Canonical (Name "elm-lang" "core") ["Platform"]) "4.0.0")),
    (["Platform","Cmd"], (C.CanonicalNameAndVersion (ECM.Canonical (Name "elm-lang" "core") ["Platform","Cmd"]) "4.0.0")),
    (["Platform","Sub"], (C.CanonicalNameAndVersion (ECM.Canonical (Name "elm-lang" "core") ["Platform","Sub"]) "4.0.0"))
  ]

main :: IO ()
main =
  -- Grab the package version so we can lookup the built modules according to compiler version
  let (Version major minor patch) = EC.version
  in let versionString = List.intercalate "." (map show [major, minor, patch])
     in
     -- Some source code to compile for now
     let source = "module Test exposing (..)\n\nx = 3"
     in
     do
       requestReadInterface <- newChan
       replyReadInterface <- newChan

       -- Start server
       forkIO $ readInterfaceService versionString requestReadInterface replyReadInterface

       compileRequestInterface <- newChan
       compileReplyInterface <- newChan

       forkIO $ compileCodeService versionString moduleVersions compileRequestInterface (requestReadInterface, replyReadInterface) compileReplyInterface

       writeChan compileRequestInterface source
       (localizer, warnings, result) <- readChan compileReplyInterface

       case result of
         Left e ->
           -- Failure, print the errors
           putStrLn
             ((List.intercalate "\n" (map (EC.errorToString localizer "" source) e)) ++ "\n")
         Right (C.CompileResult (Result docs interface js) (name,deps)) ->
           -- Success, print the javascript only for the requested module
           putStrLn (Data.Text.Lazy.unpack js)
