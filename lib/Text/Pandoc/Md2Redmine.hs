module Text.Pandoc.Md2Redmine (run, md2redmine) where

import RIO
import qualified RIO.ByteString as B
import RIO.Char (isDigit)
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
  let prepare
        | isNothing (optFrom opts) = separateFences
        | otherwise = id

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
    TextReader r -> r readerOpts . prepare $ decodeUtf8Lenient input
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

-- | Make code fences read the way GitHub reads them:
--
-- * An empty line is inserted before a fence that directly follows a line of
--   text. pandoc's Markdown would read the fence as part of the paragraph
--   when it is inside a list item.
-- * An indented fence left open is closed where its list item ends, i.e. at
--   a line indented less than the fence that starts a list item or is
--   clearly outside the item. pandoc would run the code block on to the next
--   fence.
separateFences :: Text -> Text
separateFences = T.unlines . go Nothing True . T.lines
  where
    -- the open fence, if inside a code block, and whether the previous line
    -- was blank
    go _ _ [] = []
    go (Just fence@(c, n, indent)) _ (l : ls)
      | closesFence fence l = l : go Nothing False ls
      | leavesItem indent l = T.replicate indent " " <> T.replicate n (T.singleton c) : go Nothing False (l : ls)
      | otherwise = l : go (Just fence) False ls
    go Nothing prevBlank (l : ls) = case openingFence l of
      Just fence
        | prevBlank -> l : go (Just fence) False ls
        | otherwise -> "" : l : go (Just fence) False ls
      Nothing -> l : go Nothing (T.null (T.strip l)) ls
    -- "```lang" or "~~~"; a backtick fence's info string cannot contain a
    -- backtick, which rules out a code span such as "```x``` text".
    openingFence l = case T.uncons body of
      Just (c, _)
        | c `elem` ("`~" :: String) ->
          let (marks, info) = T.span (== c) body
           in if T.length marks >= 3 && not (c == '`' && T.any (== '`') info)
                then Just (c, T.length marks, indentation l)
                else Nothing
      _ -> Nothing
      where
        body = T.stripStart l
    closesFence (c, n, _) l =
      let body = T.strip l
       in T.length body >= n && T.all (== c) body
    -- A list item's content may start up to 3 columns left of the fence.
    leavesItem fenceIndent l =
      not (T.null (T.strip l))
        && indentation l < fenceIndent
        && (indentation l + 3 < fenceIndent || startsListItem (T.stripStart l))
    startsListItem t =
      let (digits, rest) = T.span isDigit t
       in any (`T.isPrefixOf` t) ["* ", "- ", "+ "]
            || (not (T.null digits) && any (`T.isPrefixOf` rest) [". ", ") "])
    indentation = T.length . T.takeWhile (== ' ')
