-- |
--   Module      : Text.Pandoc.Writers.Textile.Redmine
--   Copyright   : Copyright (C) 2010-2021 John MacFarlane
--   License     : GNU GPL, version 2 or above
--
--   Maintainer  : Hogeyama <gan13027830@gmail.com>
--   Stability   : alpha
--   Portability : portable
--
--    Modified version of [Text.Pandoc.Writers.Textile](https://github.com/jgm/pandoc/blob/2.14.0.3/src/Text/Pandoc/Writers/Textile.hs)
--    for Redmine textile.
module Text.Pandoc.Writers.Textile.Redmine (writeTextile) where

import RIO
import RIO.List.Partial
import RIO.State
import qualified RIO.Text as T
import Text.DocLayout (literal, render)
import Text.Pandoc hiding (writeTextile)
import Text.Pandoc.ImageSize
import Text.Pandoc.Shared hiding (tshow)
import Text.Pandoc.Writers.Shared
import Text.Pandoc.XML (escapeStringForXML)

data WriterState = WriterState
  { stNotes :: [Text] -- Footnotes
  , stListLevel :: [Char] -- String at beginning of list items, e.g. "**"
  , stStartNum :: Maybe Int -- Start number if first list item
  , stUseTags :: Bool -- True if we should use HTML tags because we're in a complex list
  }

type TW = StateT WriterState

-- | Convert Pandoc to Textile.
writeTextile :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTextile opts document =
  evalStateT
    (pandocToTextile opts document)
    WriterState
      { stNotes = []
      , stListLevel = []
      , stStartNum = Nothing
      , stUseTags = False
      }

-- | Return Textile representation of document.
pandocToTextile ::
  PandocMonad m =>
  WriterOptions ->
  Pandoc ->
  TW m Text
pandocToTextile opts (Pandoc meta blocks) = do
  metadata <-
    metaToContext
      opts
      (fmap literal . blockListToTextile opts)
      (fmap literal . inlineListToTextile opts)
      meta
  body <- blockListToTextile opts blocks
  notes <- gets $ T.unlines . reverse . stNotes
  let main = body <> if T.null notes then "" else "\n\n" <> notes
  let context = defField "body" main metadata
  return $
    case writerTemplate opts of
      Nothing -> main
      Just tpl -> render Nothing $ renderTemplate tpl context

withUseTags :: PandocMonad m => TW m a -> TW m a
withUseTags action = do
  oldUseTags <- gets stUseTags
  modify $ \s -> s {stUseTags = True}
  result <- action
  modify $ \s -> s {stUseTags = oldUseTags}
  return result

-- | Escape one character as needed for Textile.
escapeCharForTextile :: Char -> Text
escapeCharForTextile x = case x of
  -- '&' -> "&amp;"
  -- '<' -> "&lt;"
  -- '>' -> "&gt;"
  -- '"' -> "&quot;"
  -- '*' -> "&#42;"
  -- '_' -> "&#95;"
  -- '@' -> "&#64;"
  -- '+' -> "&#43;"
  -- '-' -> "&#45;"
  -- '|' -> "&#124;"
  '\x2014' -> " -- "
  '\x2013' -> " - "
  '\x2019' -> "'"
  '\x2026' -> "..."
  c -> T.singleton c

-- | Escape string as needed for Textile.
escapeTextForTextile :: Text -> Text
escapeTextForTextile = T.concatMap escapeCharForTextile

-- | Convert Pandoc block element to Textile.
blockToTextile ::
  PandocMonad m =>
  -- | Options
  WriterOptions ->
  -- | Block element
  Block ->
  TW m Text
blockToTextile _ Null = return ""
-- 利便性のための hack
blockToTextile opts (Div (_, ["collapse"], keyvals) bs) = do
  contents <- blockListToTextile opts bs
  let mOpen = lookup "open" keyvals
      mClose = lookup "close" keyvals
      toggleMessage = case (mOpen, mClose) of
        (Nothing, Nothing) -> ""
        (Just open, Nothing) -> "(" <> open <> ")"
        (Nothing, Just close) -> "(" <> "表示," <> close <> ")"
        (Just open, Just close) -> "(" <> open <> "," <> close <> ")"
  -- return . T.intercalate "\n" $
  return . T.unlines $
    [ "{{collapse" <> toggleMessage
    , contents
    , "}}"
    ]
blockToTextile opts (Div attr bs) = do
  let startTag = render Nothing $ tagWithAttrs "div" attr
  let endTag = "</div>"
  contents <- blockListToTextile opts bs
  return $ startTag <> "\n\n" <> contents <> "\n\n" <> endTag <> "\n"
blockToTextile opts (Plain inlines) =
  inlineListToTextile opts inlines
-- title beginning with fig: indicates that the image is a figure
blockToTextile opts (Para [Image attr txt (src, T.stripPrefix "fig:" -> Just tit)]) = do
  inlineToTextile opts (Image attr txt (src, tit))
blockToTextile opts (Para inlines) = do
  useTags <- gets stUseTags
  listLevel <- gets stListLevel
  contents <- inlineListToTextile opts inlines
  return $
    if useTags
      then "<p>" <> contents <> "</p>"
      else contents <> if null listLevel then "\n" else ""
blockToTextile opts (LineBlock lns) =
  blockToTextile opts $ linesToPara lns
blockToTextile _ b@(RawBlock f str)
  | f == Format "html" || f == Format "textile" = return str
  | otherwise = do
    report $ BlockNotRendered b
    return ""
blockToTextile _ HorizontalRule = return "\n---\n"
blockToTextile opts (Header level (_, _, keyvals) inlines) = do
  contents <- inlineListToTextile opts inlines
  let lang = maybe "" (\x -> "[" <> x <> "]") $ lookup "lang" keyvals
  let styles = maybe "" (\x -> "{" <> x <> "}") $ lookup "style" keyvals
  let prefix = "h" <> tshow level <> styles <> lang <> ". "
  return $ prefix <> contents <> "\n"
blockToTextile _ (CodeBlock (_, classes, _) str) = do
  listLevel <- gets stListLevel
  let baseLines = ["<pre>" <> codeOpen, str, codeClose <> "</pre>"]
  return $
    if null listLevel
      then T.unlines (baseLines <> [""])
      else T.intercalate "\n" baseLines
  where
    (codeOpen, codeClose)
      | T.null (T.unwords classes) = ("", "")
      | otherwise = ("<code class=\"" <> T.unwords classes <> "\">", "</code>")
blockToTextile opts (BlockQuote blocks) = do
  contents <- blockListToTextile opts blocks
  return $ T.unlines $ map ("> " <>) $ T.lines contents
blockToTextile opts (Table _ blkCapt specs thead tbody tfoot) =
  case toLegacyTable blkCapt specs thead tbody tfoot of
    ([], aligns, widths, headers, rows') | all (== 0) widths -> do
      hs <- mapM (fmap (("_. " <>) . stripTrailingNewlines) . blockListToTextile opts) headers
      let cellsToRow cells = "|" <> T.intercalate "|" cells <> "|"
      let header = if all null headers then "" else cellsToRow hs <> "\n"
      let blocksToCell (align, bs) = do
            contents <- stripTrailingNewlines <$> blockListToTextile opts bs
            let alignMarker = case align of
                  AlignLeft -> "<. "
                  AlignRight -> ">. "
                  AlignCenter -> "=. "
                  AlignDefault -> ""
            return $ alignMarker <> contents
      let rowToCells = mapM blocksToCell . zip aligns
      bs <- mapM rowToCells rows'
      let body = T.unlines $ map cellsToRow bs
      return $ header <> body
    (capt, aligns, widths, headers, rows') -> do
      let alignStrings = map alignmentToText aligns
      captionDoc <-
        if null capt
          then return ""
          else do
            c <- inlineListToTextile opts capt
            return $ "<caption>" <> c <> "</caption>\n"
      let percent w = tshow (truncate (100 * w) :: Integer) <> "%"
      let coltags =
            if all (== 0.0) widths
              then ""
              else
                T.unlines $
                  map
                    (\w -> "<col width=\"" <> percent w <> "\" />")
                    widths
      head' <-
        if all null headers
          then return ""
          else do
            hs <- tableRowToTextile opts alignStrings 0 headers
            return $ "<thead>\n" <> hs <> "\n</thead>\n"
      body' <- zipWithM (tableRowToTextile opts alignStrings) [1 ..] rows'
      return $
        "<table>\n" <> captionDoc <> coltags <> head'
          <> "<tbody>\n"
          <> T.unlines body'
          <> "</tbody>\n</table>\n"
blockToTextile opts (BulletList items) = do
  modify $ \s -> s {stListLevel = stListLevel s <> "*"}
  level <- gets $ length . stListLevel
  contents <- mapM (listItemToTextile opts) items
  modify $ \s -> s {stListLevel = init (stListLevel s)}
  return $ vcat contents <> (if level > 1 then "" else "\n")
blockToTextile opts (OrderedList (start, _, _) items) = do
  modify $ \s ->
    s
      { stListLevel = stListLevel s <> "#"
      , stStartNum =
          if start > 1
            then Just start
            else Nothing
      }
  level <- gets $ length . stListLevel
  contents <- mapM (listItemToTextile opts) items
  modify $ \s ->
    s
      { stListLevel = init (stListLevel s)
      , stStartNum = Nothing
      }
  return $ vcat contents <> (if level > 1 then "" else "\n")
blockToTextile opts (DefinitionList items) = do
  contents <- withUseTags $ mapM (definitionListItemToTextile opts) items
  return $ "<dl>\n" <> vcat contents <> "\n</dl>\n"

-- Auxiliary functions for lists:

-- | Convert bullet or ordered list item (list of blocks) to Textile.
listItemToTextile ::
  PandocMonad m =>
  WriterOptions ->
  [Block] ->
  TW m Text
listItemToTextile opts items = do
  contents <- blockListToTextile opts items
  useTags <- gets stUseTags
  if useTags
    then return $ "<li>" <> contents <> "</li>"
    else do
      marker <- gets stListLevel
      mbstart <- gets stStartNum
      case mbstart of
        Just n -> do
          modify $ \s -> s {stStartNum = Nothing}
          return $ T.pack marker <> tshow n <> " " <> contents
        Nothing -> return $ T.pack marker <> " " <> contents

-- | Convert definition list item (label, list of blocks) to Textile.
definitionListItemToTextile ::
  PandocMonad m =>
  WriterOptions ->
  ([Inline], [[Block]]) ->
  TW m Text
definitionListItemToTextile opts (label, items) = do
  labelText <- inlineListToTextile opts label
  contents <- mapM (blockListToTextile opts) items
  return $
    "<dt>" <> labelText <> "</dt>\n"
      <> T.intercalate "\n" (map (\d -> "<dd>" <> d <> "</dd>") contents)

-- | Concatenates strings with line breaks between them.
vcat :: [Text] -> Text
vcat = T.intercalate "\n"

-- Auxiliary functions for tables. (TODO: these are common to HTML, MediaWiki,
-- and Textile writers, and should be abstracted out.)

tableRowToTextile ::
  PandocMonad m =>
  WriterOptions ->
  [Text] ->
  Int ->
  [[Block]] ->
  TW m Text
tableRowToTextile opts alignStrings rownum cols' = do
  let celltype = if rownum == 0 then "th" else "td"
  let rowclass = case rownum of
        0 -> "header"
        x | x `rem` 2 == 1 -> "odd"
        _ -> "even"
  cols'' <-
    zipWithM
      (tableItemToTextile opts celltype)
      alignStrings
      cols'
  return $ "<tr class=\"" <> rowclass <> "\">\n" <> T.unlines cols'' <> "</tr>"

alignmentToText :: Alignment -> Text
alignmentToText alignment = case alignment of
  AlignLeft -> "left"
  AlignRight -> "right"
  AlignCenter -> "center"
  AlignDefault -> "left"

tableItemToTextile ::
  PandocMonad m =>
  WriterOptions ->
  Text ->
  Text ->
  [Block] ->
  TW m Text
tableItemToTextile opts celltype align' item = do
  let mkcell x =
        "<" <> celltype <> " align=\"" <> align' <> "\">"
          <> x
          <> "</"
          <> celltype
          <> ">"
  contents <- blockListToTextile opts item
  return $ mkcell contents

-- | Convert list of Pandoc block elements to Textile.
blockListToTextile ::
  PandocMonad m =>
  -- | Options
  WriterOptions ->
  -- | List of block elements
  [Block] ->
  TW m Text
blockListToTextile opts blocks =
  vcat <$> mapM (blockToTextile opts) blocks

-- | Convert list of Pandoc inline elements to Textile.
inlineListToTextile ::
  PandocMonad m =>
  WriterOptions ->
  [Inline] ->
  TW m Text
inlineListToTextile opts lst =
  T.concat <$> mapM (inlineToTextile opts) lst

-- | Convert Pandoc inline element to Textile.
inlineToTextile :: PandocMonad m => WriterOptions -> Inline -> TW m Text
inlineToTextile opts (Span _ lst) =
  inlineListToTextile opts lst
inlineToTextile opts (Emph lst) = do
  contents <- inlineListToTextile opts lst
  return $
    if '_' `elemText` contents
      then "<em>" <> contents <> "</em>"
      else "_" <> contents <> "_"
inlineToTextile opts (Underline lst) = do
  contents <- inlineListToTextile opts lst
  return $
    if '+' `elemText` contents
      then "<u>" <> contents <> "</u>"
      else "+" <> contents <> "+"
inlineToTextile opts (Strong lst) = do
  contents <- inlineListToTextile opts lst
  return $
    if '*' `elemText` contents
      then "<strong>" <> contents <> "</strong>"
      else "*" <> contents <> "*"
inlineToTextile opts (Strikeout lst) = do
  contents <- inlineListToTextile opts lst
  return $
    if '-' `elemText` contents
      then "<del>" <> contents <> "</del>"
      else "-" <> contents <> "-"
inlineToTextile opts (Superscript lst) = do
  contents <- inlineListToTextile opts lst
  return $
    if '^' `elemText` contents
      then "<sup>" <> contents <> "</sup>"
      else "[^" <> contents <> "^]"
inlineToTextile opts (Subscript lst) = do
  contents <- inlineListToTextile opts lst
  return $
    if '~' `elemText` contents
      then "<sub>" <> contents <> "</sub>"
      else "[~" <> contents <> "~]"
inlineToTextile opts (SmallCaps lst) = inlineListToTextile opts lst
inlineToTextile opts (Quoted SingleQuote lst) = do
  contents <- inlineListToTextile opts lst
  return $ "'" <> contents <> "'"
inlineToTextile opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToTextile opts lst
  return $ "\"" <> contents <> "\""
inlineToTextile opts (Cite _ lst) = inlineListToTextile opts lst
inlineToTextile _ (Code _ str) =
  return $
    if '@' `elemText` str
      then "<tt>" <> escapeStringForXML str <> "</tt>"
      else "@" <> str <> "@"
inlineToTextile _ (Str str) = return $ escapeTextForTextile str
inlineToTextile _ (Math _ str) =
  return $ "<span class=\"math\">" <> escapeStringForXML str <> "</span>"
inlineToTextile opts il@(RawInline f str)
  | f == Format "html" || f == Format "textile" = return str
  | (f == Format "latex" || f == Format "tex")
      && isEnabled Ext_raw_tex opts =
    return str
  | otherwise = do
    report $ InlineNotRendered il
    return ""
inlineToTextile _ LineBreak = return "\n"
inlineToTextile _ SoftBreak = return " "
inlineToTextile _ Space = return " "
inlineToTextile opts (Link (_, cls, _) txt (src, _)) = do
  label <- case txt of
    [Code _ s]
      | s == src -> return s
    [Str s]
      | s == src -> return s
    _ -> inlineListToTextile opts txt
  let classes =
        if null cls || cls == ["uri"] && label == "$"
          then ""
          else "(" <> T.unwords cls <> ")"
  return $ "\"" <> classes <> label <> "\":" <> src
inlineToTextile opts (Image attr@(_, cls, _) alt (source, tit)) = do
  alt' <- inlineListToTextile opts alt
  let txt =
        if T.null tit
          then
            if T.null alt'
              then ""
              else "(" <> alt' <> ")"
          else "(" <> tit <> ")"
      classes =
        if null cls
          then ""
          else "(" <> T.unwords cls <> ")"
      showDim dir =
        let toCss str = Just $ tshow dir <> ":" <> str <> ";"
         in case dimension dir attr of
              Just (Percent a) -> toCss $ tshow (Percent a)
              Just dim -> toCss $ showInPixel opts dim <> "px"
              Nothing -> Nothing
      styles = case (showDim Width, showDim Height) of
        (Just w, Just h) -> "{" <> w <> h <> "}"
        (Just w, Nothing) -> "{" <> w <> "height:auto;}"
        (Nothing, Just h) -> "{" <> "width:auto;" <> h <> "}"
        (Nothing, Nothing) -> ""
  return $ "!" <> classes <> styles <> source <> txt <> "!"
inlineToTextile opts (Note contents) = do
  curNotes <- gets stNotes
  let newnum = length curNotes + 1
  contents' <- blockListToTextile opts contents
  let thisnote = "fn" <> tshow newnum <> ". " <> contents' <> "\n"
  modify $ \s -> s {stNotes = thisnote : curNotes}
  return $ "[" <> tshow newnum <> "]"
