{-# LANGUAGE DeriveGeneric #-}

module Text.Pandoc.Writers.Textile.RedmineSpec (spec) where

import Data.Aeson (FromJSON, eitherDecodeFileStrict')
import GHC.IO (unsafePerformIO)
import RIO
import RIO.Directory (doesDirectoryExist, listDirectory)
import RIO.List (sort)
import qualified RIO.Text as T
import Test.Hspec
import Test.Hspec.Golden
import Text.Pandoc.Md2Redmine
import qualified Prelude (readFile, writeFile)

-- | A group of golden test cases, as listed in test/cases/cases.json.
data CaseGroup = CaseGroup
  { title :: Text
  , cases :: [Case]
  }
  deriving (Generic)

instance FromJSON CaseGroup

data Case = Case
  { name :: FilePath
  , description :: Text
  }
  deriving (Generic)

instance FromJSON Case

spec :: Spec
spec = do
  groups :: [CaseGroup] <-
    runIO $ either throwString pure =<< eitherDecodeFileStrict' (dir <> "/cases.json")
  forM_ groups $ \g ->
    describe (T.unpack (title g)) $
      forM_ (cases g) $ \c ->
        it (name c <> ": " <> T.unpack (description c)) $
          goldenTest (name c)

  describe "cases.json" $
    it "should list every case directory" $ do
      entries <- listDirectory dir
      caseDirs <- filterM (doesDirectoryExist . ((dir <> "/") <>)) entries
      sort (concatMap (map name . cases) groups) `shouldBe` sort caseDirs

dir :: FilePath
dir = "test/cases"

goldenTest :: FilePath -> Golden String
goldenTest testName = unsafePerformIO $ do
  input <- readFileUtf8 (dir <> "/" <> testName <> "/input.md")
  result <- md2redmine input
  return $
    Golden
      { output = T.unpack result
      , encodePretty = id
      , writeToFile = Prelude.writeFile
      , readFromFile = Prelude.readFile
      , goldenFile = dir <> "/" <> testName <> "/golden"
      , actualFile = Just (dir <> "/" <> testName <> "/actual")
      , failFirstTime = False
      }
