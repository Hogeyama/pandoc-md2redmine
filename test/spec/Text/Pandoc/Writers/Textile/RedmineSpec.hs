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

    it "case04: should render pipe tables as Textile even when widths are inferred" $
      goldenTest "case04"

  describe "Redmine's RedCloth rules (Golden tests)" $ do
    it "case05: should escape characters that Redmine would take as markup" $
      goldenTest "case05"

    it "case06: should separate inline markup from adjacent Japanese text" $
      goldenTest "case06"

    it "case07: should not insert a space at line breaks between Japanese characters" $
      goldenTest "case07"

    it "case08: should escape delimiters inside markup and use <code> where @ does not work" $
      goldenTest "case08"

    it "case09: should keep table cells on one row and put the caption before the table" $
      goldenTest "case09"

    it "case10: should not emit start numbers of ordered lists" $
      goldenTest "case10"

    it "case11: should end a figure paragraph and link images" $
      goldenTest "case11"

    it "case12: should recognize blocks without a preceding blank line and keep dashes" $
      goldenTest "case12"

    it "case13: should keep all paragraphs of a footnote in the footnote" $
      goldenTest "case13"

    it "case14: should render definition lists without HTML tags" $
      goldenTest "case14"

    it "case15: should link to headings with Redmine's anchor names" $
      goldenTest "case15"

    it "case16: should drop raw HTML that Redmine would show as text" $
      goldenTest "case16"

    it "case17: should render autolinks and link titles" $
      goldenTest "case17"

    it "case18: should ignore a hard line break at the end of a list item or paragraph" $
      goldenTest "case18"

    it "case19: should keep a list going after a collapse macro, and end it before the next block" $
      goldenTest "case19"

    it "case20: should turn <details> into the collapse macro, expanding nested ones" $
      goldenTest "case20"

    it "case21: should render grid and multiline tables with a caption as Textile tables" $
      goldenTest "case21"

    it "case22: should turn mermaid code blocks into the mermaid macro where a macro can hold them" $
      goldenTest "case22"

    it "case23: should start a code block at a fence right after a line of text" $
      goldenTest "case23"

    it "case24: should close an unclosed code fence where its list item ends" $
      goldenTest "case24"

    it "case25: should continue text right after a code block in a list item" $
      goldenTest "case25"

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
