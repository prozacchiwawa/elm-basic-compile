{-# OPTIONS_GHC -Wall #-}

module Main where

import JSInterface as JSI

import           Control.Concurrent
import qualified Data.List           as List
import qualified Data.Text.Lazy

import           Elm.Compiler as EC
import           Elm.Compiler.Module as ECM
import           Elm.Package

moduleVersions = []

main :: IO ()
main = do
  return ()
