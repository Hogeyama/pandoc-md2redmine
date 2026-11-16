module Text.Pandoc.Md2Redmine (run, md2redmine) where

import RIO
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import Text.Pandoc as Pandoc hiding (writeTextile)
import Text.Pandoc.App as Pandoc
import Text.Pandoc.Shared (eastAsianLineBreakFilter)
import Text.Pandoc.Writers.Textile.Redmine

-- | Command line entry point. Accepts pandoc's options; @-f@ overrides the
-- default Markdown dialect and @-o@ selects the output file.
run :: IO ()
run = do
  opts <- parseOptions options defaultOpts
  out <- handleError =<< runIO (readInputs opts >>= convert opts)
  case optOutputFile opts of
    Just path | path /= "-" -> writeFileUtf8 path out
    _ -> B.putStr (encodeUtf8 out)

-- | Convert Markdown text to Redmine Textile with the default options.
md2redmine :: Text -> IO Text
md2redmine input =
  handleError =<< runIO (convert defaultOpts (encodeUtf8 input))

-- | Read all input files (stdin when none or @-@ is given). Each file is
-- terminated by a newline so that the last line of one file does not merge
-- into the first line of the next.
readInputs :: Opt -> PandocIO ByteString
readInputs opts = case optInputFiles opts of
  Nothing -> liftIO $ B.hGetContents stdin
  Just paths -> mconcat <$> mapM (fmap ensureNewline . readPath) paths
  where
    readPath "-" = liftIO $ B.hGetContents stdin
    readPath path = liftIO $ B.readFile path
    ensureNewline bs
      | B.null bs || "\n" `B.isSuffixOf` bs = bs
      | otherwise = bs <> "\n"

convert :: Opt -> ByteString -> PandocIO Text
convert opts input = do
  (reader, exts) <- case optFrom opts of
    Nothing -> (,redmineMarkdownExtensions) . fst <$> getReader "markdown"
    Just format -> getReader format

  let readerOpts =
        Pandoc.def
          { readerStandalone = False
          , readerColumns = optColumns opts
          , readerTabStop = optTabStop opts
          , readerIndentedCodeClasses = optIndentedCodeClasses opts
          , readerDefaultImageExtension = optDefaultImageExtension opts
          , readerTrackChanges = optTrackChanges opts
          , readerStripComments = optStripComments opts
          , readerExtensions = exts
          }

  doc <- case reader of
    TextReader r -> r readerOpts $ decodeUtf8Lenient input
    ByteStringReader r -> r readerOpts $ fromStrictBytes input

  let doc'
        | extensionEnabled Ext_east_asian_line_breaks exts = eastAsianLineBreakFilter doc
        | otherwise = doc

  let writerOpts =
        def
          { writerTabStop = optTabStop opts
          , writerTableOfContents = optTableOfContents opts
          , writerHTMLMathMethod = optHTMLMathMethod opts
          , writerIncremental = optIncremental opts
          , writerCiteMethod = optCiteMethod opts
          , writerNumberSections = optNumberSections opts
          , writerNumberOffset = optNumberOffset opts
          , writerSectionDivs = optSectionDivs opts
          , writerExtensions = getDefaultExtensions "textile"
          , writerReferenceLinks = optReferenceLinks opts
          , writerReferenceLocation = optReferenceLocation opts
          , writerDpi = optDpi opts
          , writerWrapText = optWrap opts
          , writerColumns = optColumns opts
          , writerEmailObfuscation = optEmailObfuscation opts
          , writerIdentifierPrefix = optIdentifierPrefix opts
          , writerHtmlQTags = optHtmlQTags opts
          , writerTopLevelDivision = optTopLevelDivision opts
          , writerListings = optListings opts
          , writerSlideLevel = optSlideLevel opts
          , writerSetextHeaders = optSetextHeaders opts
          , writerEpubSubdirectory = T.pack $ optEpubSubdirectory opts
          , writerEpubFonts = optEpubFonts opts
          , writerEpubChapterLevel = optEpubChapterLevel opts
          , writerTOCDepth = optTOCDepth opts
          , writerReferenceDoc = optReferenceDoc opts
          , writerPreferAscii = optAscii opts
          }

  writeTextile writerOpts doc'

-- | pandoc's Markdown, adjusted for documents written with GitHub-style
-- habits and Japanese text:
--
-- * headings, block quotes and lists are recognized without a preceding
--   blank line;
-- * a line break between two East Asian characters does not become a space;
-- * @--@ and quotes are kept as typed instead of becoming typographic
--   punctuation.
redmineMarkdownExtensions :: Extensions
redmineMarkdownExtensions =
  enableExtension Ext_east_asian_line_breaks
    . enableExtension Ext_lists_without_preceding_blankline
    . disableExtension Ext_blank_before_header
    . disableExtension Ext_blank_before_blockquote
    . disableExtension Ext_smart
    $ getDefaultExtensions "markdown"
