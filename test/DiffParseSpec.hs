{-# LANGUAGE OverloadedStrings #-}

module DiffParseSpec (spec) where

import CheckLines
import Control.Monad (zipWithM_)
import Data.ByteString qualified as BS
import Data.Either (isRight)
import DiffParse
import System.FilePath (addExtension, takeBaseName, (</>))
import System.FilePath.Glob (compile, globDir1)
import Test.Hspec
import Test.Hspec.Golden

samplesDir, goldenDir :: FilePath
samplesDir = "samples"
goldenDir = "test" </> "goldenFiles"

spec :: Spec
spec = do
  describe "EmptyDiff parser" $ do
    it "Parses empty diff" $ show (diffParse "") `shouldBe` "Right Empty Diff"
  describe "NormalDiff parser" $ do
    (names, eDiffs) <- runIO $ getDiffs "*-n.diff"
    zipWithM_ isDiff names eDiffs
    zipWithM_ goldenTests names eDiffs
  describe "UniDiff parser" $ do
    (names, eDiffs) <- runIO $ getDiffs "*-u.diff"
    zipWithM_ isDiff names eDiffs
    zipWithM_ checkUniLines names eDiffs
    zipWithM_ goldenTests names eDiffs
  describe "GitDiff parser" $ do
    (names, eDiffs) <- runIO $ getDiffs "*-g.diff"
    zipWithM_ isDiff names eDiffs
    zipWithM_ checkUniLines names eDiffs
    zipWithM_ goldenTests names eDiffs

getDiffs :: String -> IO ([String], [Either String Diff])
getDiffs regexp = do
  files <- globDir1 (compile regexp) samplesDir
  let names = map takeBaseName files
  eDiffs <- mapM (fmap diffParse . BS.readFile) files
  pure (names, eDiffs)

isDiff :: String -> Either String Diff -> Spec
isDiff name eDiff =
  it ("Parses to Diff " <> name) $ isRight eDiff `shouldBe` True

goldenTests :: String -> Either String Diff -> Spec
goldenTests name eDiff =
  it ("Parses " <> name) $
    (defaultGolden name (show eDiff))
      { goldenFile = goldenName
      , actualFile = actualName
      }
 where
  goldenName = goldenDir </> addExtension name "golden"
  actualName = Just $ goldenDir </> addExtension name "actual"
