module Text.Pandoc.Writers.Textile.RedmineSpec (spec) where

import GHC.IO (unsafePerformIO)
import RIO
import qualified RIO.Text as T
import Test.Hspec
import Test.Hspec.Golden
import Text.Pandoc.Md2Redmine
import qualified Prelude (readFile, writeFile)

spec :: Spec
spec = do
  describe "CodeBlock in lists (Golden tests)" $ do
    it "case01: should not add extra blank lines between consecutive list items with code blocks" $
      goldenTest "case01"

    it "case02: should add blank line between code block and header" $
      goldenTest "case02"

    it "case03: should handle code blocks with language specification" $
      goldenTest "case03"

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
      , testName = testName
      , failFirstTime = False
      , directory = dir
      }
  where
    dir = "test/cases"
