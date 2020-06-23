{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}

module Main where

-- TODO: use http://hackage.haskell.org/package/managed instead of turtle

-- TODO
-- dont use system-filepath (Filesystem.Path module, good lib, turtle is using it,         FilePath is just record)
-- dont use filepath        (System.FilePath module, bad lib,  directory-tree is using it, FilePath is just String)
-- use https://hackage.haskell.org/package/path-io-1.6.0/docs/Path-IO.html walkDirAccumRel

-- import qualified Filesystem.Path.CurrentOS
import "protolude" Protolude
import "base" Data.String
import "base" Data.List
import "text" Data.Text

cssModuleFileContent = [text|
  .myButton {
    color: green;
  }

  .myButton > a {
    color: green;
  }

  .myButton2 > a {
    color: green;
  }

  #myButton3 > a {
    color: green;
  }

  @media print {
    * {
      text-shadow: none !important;
      color: #000 !important;
    }

    a, a:visited { text-decoration: underline; }

    .myButton3 > a {
      color: green;
    }
  }
|]

main :: IO ()
main = putStrLn "Test suite not yet implemented"
