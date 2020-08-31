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
import qualified "system-filepath" Filesystem.Path
import "base" Data.String (String)
import qualified "base" Data.String as String
import qualified "base" Data.List as List
import qualified Data.List.Index as List
import qualified "text" Data.Text as Text
import qualified Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified "directory-tree" System.Directory.Tree
import "directory-tree" System.Directory.Tree (DirTree (..), AnchoredDirTree (..))
import qualified "cases" Cases
import Control.Concurrent.Async
import CssContentToTypes

newtype PathToModule = PathToModule { unPathToModule :: NonEmpty Text }
  deriving (Show)

filterDirTreeByFilename :: (String -> Bool) -> DirTree a -> Bool
filterDirTreeByFilename _ (Dir ('.':_) _) = False
filterDirTreeByFilename pred (File n _) = pred n
filterDirTreeByFilename _ _ = True

dirTreeContent :: DirTree a -> IO [a]
dirTreeContent (Failed name err) = Turtle.die $ "Dir tree error: filename " <> show name <> ", error " <> show err
dirTreeContent (File fileName a) = pure [a]
dirTreeContent (Dir dirName contents) = do
  output :: [[a]] <- traverse dirTreeContent contents
  pure $ join output

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
    <> help "Base dir with Anyname.module.css OR Anyname.module.scss files" )

appOptionsParserInfo :: ParserInfo AppOptions
appOptionsParserInfo = info (appOptionsParser <**> helper)
  ( fullDesc
  <> progDesc "Halogen FFI generator for webpack css modules"
  <> header "Based on Anyname.module.css, generates Anyname.purs and Anyname.js files" )

-- Example:
-- baseDir - /home/srghma/projects/purescript-halogen-nextjs/app/
-- filePath - /home/srghma/projects/purescript-halogen-nextjs/app/Nextjs/Pages/Buttons/CSS.module.css
-- output - ["Nextjs","Pages","Buttons","CSS"]
fullPathToPathToModule :: Turtle.FilePath -> Turtle.FilePath -> IO PathToModule
fullPathToPathToModule baseDir fullPath = do
  fullPath'' :: Turtle.FilePath <- maybe (Turtle.die $ "Cannot strip baseDir " <> show baseDir <> " from path " <> show fullPath) pure $ Turtle.stripPrefix baseDir fullPath
  let modulePathWithoutRoot :: [Text] = fmap (toS . stripSuffix "/" . Turtle.encodeString) . Turtle.splitDirectories . Filesystem.Path.dropExtensions $ fullPath''

  modulePathWithoutRoot' :: NonEmpty Text <- maybe (Turtle.die $ "should be nonEmpty modulePathWithoutRoot for" <> show baseDir <> " from path " <> show fullPath) pure $ NonEmpty.nonEmpty modulePathWithoutRoot

  pure (PathToModule modulePathWithoutRoot')

appendIfNotAlreadySuffix :: Eq a => [a] -> [a] -> [a]
appendIfNotAlreadySuffix suffix target =
  if List.isSuffixOf suffix target
     then target
     else target ++ suffix

stripSuffix :: Eq a => [a] -> [a] -> [a]
stripSuffix suffix target =
  if List.isSuffixOf suffix target
     then List.reverse $ List.drop (List.length suffix) $ List.reverse target
     else target

-- make it end with /
makeValidDirectory :: Turtle.FilePath -> Turtle.FilePath
makeValidDirectory = Turtle.decodeString . appendIfNotAlreadySuffix "/" . Turtle.encodeString

main :: IO ()
main = do
  appOptions <- execParser appOptionsParserInfo

  let baseDir = makeValidDirectory $ directory appOptions -- ending with /

  -- liftIO $ putStrLn $ "baseDir " <> Turtle.encodeString baseDir

  -- contains absolute path inside
  _base :/ (dirTree :: DirTree FilePath) <- liftIO $ System.Directory.Tree.readDirectoryWith return (Turtle.encodeString baseDir)

  let (dirTreeWithCssFiles :: DirTree FilePath) =
        System.Directory.Tree.filterDir
          (filterDirTreeByFilename (\n -> System.FilePath.takeExtensions n == ".module.css" || System.FilePath.takeExtensions n == ".module.scss"))
          dirTree

  filePaths :: [Turtle.FilePath] <- map Turtle.decodeString <$> dirTreeContent dirTreeWithCssFiles

  forConcurrently_ filePaths \(filePath) -> Turtle.sh $ do
    liftIO $ putStrLn $ "processing " <> Turtle.encodeString filePath

    cssFileContent <- liftIO $ Turtle.readTextFile filePath

    let (classNames :: [Text]) = cssContentToTypes cssFileContent

    pathToModule <- liftIO $ fullPathToPathToModule baseDir filePath

    -- liftIO $ putStrLn @Text $ "output " <> show classNames
    -- liftIO $ putStrLn @Text $ "pathToModule " <> show pathToModule

    let fileModuleName :: Text = NonEmpty.last (unPathToModule pathToModule)

    let jsFilePath :: Turtle.FilePath = Turtle.directory filePath Turtle.</> Turtle.decodeString (toS fileModuleName) Turtle.<.> "js"
    let pursFilePath :: Turtle.FilePath = Turtle.directory filePath Turtle.</> Turtle.decodeString (toS fileModuleName) Turtle.<.> "purs"

    liftIO $ putStrLn $ "  writing " <> Turtle.encodeString jsFilePath
    liftIO $ putStrLn $ "  writing " <> Turtle.encodeString pursFilePath

    liftIO $ Turtle.writeTextFile jsFilePath
      ( Text.unlines
        [ "// Do not edit, this file was autogenerated by generate-halogen-css-modules"
        , "exports.styles = require('./" <> toS (Turtle.encodeString (Turtle.filename filePath)) <> "')"
        ]
      )
    liftIO $ Turtle.writeTextFile pursFilePath
      ( let
          imports = Text.unlines $ List.imap (\(index) (className :: Text) -> (if index == 0 then "  { " else "  , ") <> className <> " :: ClassName") classNames
          styles =
            if List.length classNames == 0
               then "foreign import styles :: {}"
               else "foreign import styles ::\n" <> imports <> "  }"
        in Text.unlines
          [ "-- Do not edit, this file was autogenerated by generate-halogen-css-modules"
          , "module " <> Text.intercalate "." (NonEmpty.toList (unPathToModule pathToModule)) <> " (styles) where"
          , ""
          , "import Halogen.HTML (ClassName)"
          , ""
          , styles
          ]
      )
