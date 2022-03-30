module Text.Pandoc.Md2Redmine (run) where

import RIO
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import Text.Pandoc as Pandoc hiding (writeTextile)
import Text.Pandoc.App as Pandoc

import Text.Pandoc.Writers.Textile.Redmine

run :: IO Text
run = do
  opts <- parseOptions options defaultOpts

  -- main
  handleError =<< do
    runIO $ do
      (reader, exts) <- getReader "markdown"

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

      input <- liftIO $ case optInputFiles opts of
        Nothing -> B.hGetContents stdin
        Just fs -> mconcat <$> mapM B.readFile fs

      doc <- case reader of
        TextReader r -> r readerOpts $ decodeUtf8Lenient input
        ByteStringReader r -> r readerOpts $ fromStrictBytes input

      debug "-------------------------------------------------------------\n"
      debug . (<> "\n") . encodeUtf8 $ tshow doc

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

      out <- writeTextile writerOpts doc

      debug "-------------------------------------------------------------\n"
      debug . (<> "\n") . encodeUtf8 $ out

      pure out

debug :: MonadIO m => ByteString -> m ()
debug s = if True then B.putStr s else pure ()
