{-# OPTIONS_GHC -Wall #-}

module Main where

import Compiler
import JSInterface

import           Control.Concurrent
import qualified Data.List           as List
import qualified Data.Text.Lazy

import           Elm.Compiler
import           Elm.Compiler.Module
import           Elm.Package

main :: IO ()
main =
  -- Grab the package version so we can lookup the built modules according to compiler version
  let (Version major minor patch) = Elm.Compiler.version
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

       forkIO $ compileCodeService versionString compileRequestInterface (requestReadInterface, replyReadInterface) compileReplyInterface

       writeChan compileRequestInterface source
       (localizer, warnings, result) <- readChan compileReplyInterface

       case result of
         Left e ->
           -- Failure, print the errors
           putStrLn
             ((List.intercalate "\n" (map (Elm.Compiler.errorToString localizer "" source) e)) ++ "\n")
         Right (Result docs interface js) ->
           -- Success, print the javascript only for the requested module
           putStrLn (Data.Text.Lazy.unpack js)
