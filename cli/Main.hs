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

-- TODO
-- use https://hackage.haskell.org/package/recursion-schemes

-- import qualified Filesystem.Path.CurrentOS
import Options.Applicative
import "protolude" Protolude hiding (find)
import qualified "turtle" Turtle
import "turtle" Turtle ((</>))
import qualified "directory" System.Directory
import qualified "filepath" System.FilePath
import "base" Data.String
import "base" Data.List
import "text" Data.Text
import qualified "foldl" Control.Foldl
import qualified "directory-tree" System.Directory.Tree
import "directory-tree" System.Directory.Tree (DirTree (..), AnchoredDirTree (..))
import qualified "cases" Cases
import Control.Concurrent.Async
import CssContentToTypes

newtype PathToModule = PathToModule [Text]

filterDirTreeByFilename :: (String -> Bool) -> DirTree a -> Bool
filterDirTreeByFilename _ (Dir ('.':_) _) = False
filterDirTreeByFilename pred (File n _) = pred n
filterDirTreeByFilename _ _ = True

dirTreeToFileList :: DirTree a -> IO [(Turtle.FilePath, a)]
dirTreeToFileList (Failed name err) = Turtle.die $ "Dir tree error: filename " <> show name <> ", error " <> show err
dirTreeToFileList (File name a) = pure [(Turtle.decodeString name, a)]
dirTreeToFileList (Dir name contents) = do
  output :: [[(Turtle.FilePath, a)]] <- traverse dirTreeToFileList contents
  pure (join output)

anyCaseToCamelCase :: Text -> Text
anyCaseToCamelCase = Cases.process Cases.title Cases.camel -- first letter is always upper

data AppOptions = AppOptions
  { directory :: Turtle.FilePath
  }

appOptionsParser :: Parser AppOptions
appOptionsParser = AppOptions
  <$> strOption
      ( long "directory"
    <> short 'd'
    <> metavar "DIRECTORY"
    <> help "Base dir with Anyname.module.css files" )

appOptionsParserInfo :: ParserInfo AppOptions
appOptionsParserInfo = info (appOptionsParser <**> helper)
  ( fullDesc
  <> progDesc "Halogen FFI generator for webpack css modules"
  <> header "Based on Anyname.module.css, generates Anyname.purs and Anyname.js files" )

fullPathToPathToModule :: Turtle.FilePath -> System.FilePath.FilePath -> IO PathToModule
fullPathToPathToModule baseDir fullPath = do
  let fullPath' :: Turtle.FilePath = Turtle.decodeString fullPath
  fullPath'' :: Turtle.FilePath <- maybe (Turtle.die $ "Cannot strip baseDir " <> show baseDir <> " from path " <> show fullPath) pure $ Turtle.stripPrefix baseDir fullPath'
  let modulePathWithoutRoot :: [Text] = fmap (toS . Turtle.encodeString)  . Turtle.splitDirectories . Turtle.dropExtension $ fullPath''
  pure (PathToModule modulePathWithoutRoot)

main :: IO ()
main = do
  appOptions <- execParser appOptionsParserInfo

  _base :/ (dirTree :: DirTree PathToModule) <- liftIO $ System.Directory.Tree.readDirectoryWith (fullPathToPathToModule (directory appOptions)) (Turtle.encodeString (directory appOptions))

  let (dirTreeWithCssFiles :: DirTree PathToModule) =
        System.Directory.Tree.filterDir
          (filterDirTreeByFilename (\n -> System.FilePath.takeExtensions n == ".module.css"))
          dirTree

  filePaths :: [(Turtle.FilePath, PathToModule)] <- liftIO $ dirTreeToFileList dirTreeWithCssFiles

  forConcurrently_ filePaths \(filePath, pathToModule) -> Turtle.sh $ do
    liftIO $ putStrLn $ "processing " <> Turtle.encodeString filePath

    cssFileContent <- liftIO $ Turtle.readTextFile filePath

    let (classNames :: [Text]) = cssContentToTypes cssFileContent

    liftIO $ putStrLn @Text $ "output " <> show classNames
    -- liftIO $ Turtle.writeTextFile (projectRoot </> "test/" </> "AllTests.purs") fileContent

    pure ()

    -- let imports = Data.Text.unlines $ specNameList <&> (
    --       \(specName :: SpecName) ->
    --         let specPath = Data.Text.intercalate "." specName
    --         in "import " <> specPath <> " as " <> specPath
    --       )

    -- let specsWrappedInDecribesAndIt :: Text = specTreeToSpecsWrappedInDecribesAndIt (removeCommonLayer specTree)

    -- let fileContent :: Text = Data.Text.unlines
    --       [ "module Test.AllTests where"
    --       , ""
    --       , "import Prelude"
    --       , ""
    --       , "import Test.Spec (describe)"
    --       , ""
    --       , "import Lib.FeatureTest (FeatureTestSpecInternal, it)"
    --       , ""
    --       , imports
    --       , ""
    --       , "allTests :: FeatureTestSpecInternal Unit"
    --       , "allTests = do"
    --       , specsWrappedInDecribesAndIt
    --       ]

    -- -- liftIO $ putStrLn fileContent
    -- liftIO $ Turtle.writeTextFile (projectRoot </> "test/" </> "AllTests.purs") fileContent

    -- return ()
