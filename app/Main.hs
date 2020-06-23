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

type PathToModule = [Text]

data SpecTree
  = Describe Text [SpecTree]
  | It Text PathToModule
  deriving (Show)

anyCaseToCamelCase :: Text -> Text
anyCaseToCamelCase = Cases.process Cases.title Cases.camel -- first letter is always upper

dirTreeToSpecTree :: DirTree [Text] -> IO SpecTree
dirTreeToSpecTree (Failed name err) = Turtle.die $ "Dir tree error: filename " <> show name <> ", error " <> show err
dirTreeToSpecTree (File name pathToModule) =
  let name' = anyCaseToCamelCase . toS . System.FilePath.takeBaseName $ name
  in pure $ It name' pathToModule
dirTreeToSpecTree (Dir name contents) = do
  output :: [SpecTree] <- traverse dirTreeToSpecTree contents
  let name' = anyCaseToCamelCase . toS $ name
  pure $ Describe name' output

filterDirTreeByFilename :: (String -> Bool) -> DirTree a -> Bool
filterDirTreeByFilename _ (Dir ('.':_) _) = False
filterDirTreeByFilename pred (File n _) = pred n
filterDirTreeByFilename _ _ = True

type SpecName = [Text] -- e.g. [ FeatureTests, Register, SuccessSpec ]

{-
  Example

  > Describe "Registration" [It "Test1", It "Test2"]
  [ ["registration", "test1"]
  , ["registration", "test2"]
  ]
-}
specTreeToList :: SpecTree -> [SpecName]
specTreeToList (It name _pathToModule) = [[name]]
specTreeToList (Describe name treeArr) =
  let output :: [SpecName] = specTreeToList =<< treeArr
   in fmap (\(specName :: SpecName) -> name:specName ) output

{-
  Example

  > [Describe "Registration" [It "Test1", It "Test2"]]

  """
  describe "registration" do
    it "test1" FeatureTests.Test1.spec
    it "test2" FeatureTests.Test2.spec
  """

  > [Describe "registration" []]

  """
  describe "registration" do
    pure unit
  """
-}

specTreeToSpecsWrappedInDecribesAndIt :: [SpecTree] -> Text
specTreeToSpecsWrappedInDecribesAndIt specTreeArr = Data.Text.unlines $ go =<< specTreeArr
  where
    appendTab :: Text -> Text
    appendTab = Data.Text.append "  "

    go :: SpecTree -> [Text]
    go (It name fullPath) =
      let moduleNames :: [Text] = fmap (Cases.process Cases.title Cases.camel) fullPath
          name' :: Text         = Cases.process Cases.lower Cases.whitespace $ fromMaybe name $ Data.Text.stripSuffix "Spec" name
       in pure $ appendTab $ "it \"" <> name' <> "\" " <> Data.Text.intercalate "." moduleNames <> ".spec"
    go (Describe name treeArr) =
      let output :: [Text]     = fmap appendTab $ go =<< treeArr
          name' :: Text        = Cases.process Cases.lower Cases.whitespace name
          describe :: Text     = appendTab $ "describe \"" <> name' <> "\" do"
       in describe:output


removeCommonLayer :: SpecTree -> [SpecTree]
removeCommonLayer tree = go [tree]
  where
    go :: [SpecTree] -> [SpecTree]
    go [Describe _name treeArr] = go treeArr
    go treeArr = treeArr

main :: IO ()
main = Turtle.sh $ do
  projectRoot :: Turtle.FilePath <- Turtle.pwd

  let testsDir :: Turtle.FilePath = projectRoot </> "src/FeatureTests/"
  let moduleBaseDir :: Turtle.FilePath = projectRoot </> "src/"

  let fullPathToPathToModule :: System.FilePath.FilePath -> IO PathToModule
      fullPathToPathToModule fullPath = do
        let fullPath' :: Turtle.FilePath = Turtle.decodeString fullPath
        let base :: Turtle.FilePath = moduleBaseDir
        fullPath'' :: Turtle.FilePath <- maybe (Turtle.die $ "Cannot stripe base " <> show base <> " from path " <> show fullPath) pure $ Turtle.stripPrefix base fullPath'
        let modulePathWithoutRoot :: [Text] = fmap (toS . Turtle.encodeString)  . Turtle.splitDirectories . Turtle.dropExtension $ fullPath''
        -- traceShowM modulePathWithoutRoot
        let modulePathWithoutRoot' :: [Text] = fmap anyCaseToCamelCase modulePathWithoutRoot
        -- traceShowM modulePathWithoutRoot'
        pure modulePathWithoutRoot'


  _base :/ (dirTree :: DirTree [Text]) <- liftIO $ System.Directory.Tree.readDirectoryWith fullPathToPathToModule (Turtle.encodeString testsDir)

  let (dirTreeWithOnlyPurescriptFiles :: DirTree [Text]) =
        System.Directory.Tree.filterDir
          (filterDirTreeByFilename
            (\n ->
              System.FilePath.takeExtension n == ".purs" &&
              "-spec" `Data.List.isSuffixOf` System.FilePath.takeBaseName n
            )
          )
          dirTree

  specTree :: SpecTree <- liftIO $ dirTreeToSpecTree dirTreeWithOnlyPurescriptFiles
  let specNameList :: [SpecName] = specTreeToList specTree

  liftIO $ print specTree
  liftIO $ print specNameList

  let imports = Data.Text.unlines $ specNameList <&> (
        \(specName :: SpecName) ->
          let specPath = Data.Text.intercalate "." specName
          in "import " <> specPath <> " as " <> specPath
        )

  let specsWrappedInDecribesAndIt :: Text = specTreeToSpecsWrappedInDecribesAndIt (removeCommonLayer specTree)

  let fileContent :: Text = Data.Text.unlines
        [ "module Test.AllTests where"
        , ""
        , "import Prelude"
        , ""
        , "import Test.Spec (describe)"
        , ""
        , "import Lib.FeatureTest (FeatureTestSpecInternal, it)"
        , ""
        , imports
        , ""
        , "allTests :: FeatureTestSpecInternal Unit"
        , "allTests = do"
        , specsWrappedInDecribesAndIt
        ]

  -- liftIO $ putStrLn fileContent
  liftIO $ Turtle.writeTextFile (projectRoot </> "test/" </> "AllTests.purs") fileContent

  return ()
