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
--
--    Redmine renders Textile with its own fork of RedCloth 3
--    (@lib/redmine/wiki_formatting/textile/redcloth3.rb@). The output of this
--    writer follows the rules of that parser:
--
--    * HTML tags other than @pre@, @code@, @kbd@ and @notextile@ are shown
--      as literal text, so everything is expressed in Textile syntax.
--    * Inline markup such as @*strong*@ is recognized only when the opening
--      mark follows whitespace, @>@ or @(@, and the closing mark is followed
--      by whitespace, punctuation, @<@ or @)@. Where the neighboring text does
--      not satisfy this (typically Japanese text), an empty
--      @\<notextile>\</notextile>@ is placed between them.
--    * Characters in plain text that the parser would take as markup are
--      written as numeric character references.
module Text.Pandoc.Writers.Textile.Redmine (writeTextile) where

import Data.Char (GeneralCategory (ConnectorPunctuation, DecimalNumber, EnclosingMark, LetterNumber, LowercaseLetter, ModifierLetter, NonSpacingMark, OtherLetter, SpacingCombiningMark, TitlecaseLetter, UppercaseLetter), generalCategory, isAlphaNum, isAscii, isDigit, isPunctuation, ord)
import RIO
import RIO.List (find, unzip, zipWith3)
import RIO.List.Partial (init)
import qualified RIO.Map as Map
import RIO.State
import qualified RIO.Text as T
import Text.DocLayout (literal, render)
import Text.Pandoc hiding (writeTextile)
import Text.Pandoc.ImageSize
import Text.Pandoc.Shared hiding (tshow)
import Text.Pandoc.Walk (query, walk)
import Text.Pandoc.Writers.Shared

data WriterState = WriterState
  { stNotes :: [Text] -- Footnotes
  , stListLevel :: [Char] -- String at beginning of list items, e.g. "**"
  , stEscapeChars :: [Char] -- Delimiters of the enclosing inline markup; they must not appear raw in text
  , stAnchors :: Map Text Text -- Pandoc header identifier -> anchor name Redmine gives the heading
  , stInCollapse :: Bool -- True inside a collapse macro
  }

type TW = StateT WriterState

-- | Convert Pandoc to Textile.
writeTextile :: PandocMonad m => WriterOptions -> Pandoc -> m Text
writeTextile opts document =
  resolveBoundaries
    <$> evalStateT
      (pandocToTextile opts document')
      WriterState
        { stNotes = []
        , stListLevel = []
        , stEscapeChars = []
        , stAnchors = headerAnchors blocks
        , stInCollapse = False
        }
  where
    document'@(Pandoc _ blocks) = walk groupDetails document

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

------------------------------------------------------------------------------
-- Boundaries around inline markup

-- | Placeholders emitted next to inline markup. Each one states a condition
-- on the adjacent character of the final output. 'resolveBoundaries' removes
-- them, inserting 'boundaryGuard' where the condition does not hold.
-- Unicode noncharacters are used so that they never collide with input text.
spanOpen, spanClose, codeEdge, linkEnd, imageStart :: Char
spanOpen = '\xFDD0'
spanClose = '\xFDD1'
codeEdge = '\xFDD2'
linkEnd = '\xFDD3'
imageStart = '\xFDD4'

data BoundaryCheck = CheckPrev (Char -> Bool) | CheckNext (Char -> Bool)

boundaryCheck :: Char -> Maybe BoundaryCheck
boundaryCheck c
  -- '|' is accepted because RedCloth turns table cells into <td> before it
  -- reads inline markup; a '|' in text is always escaped by this writer.
  | c == spanOpen = Just $ CheckPrev \p -> isRubySpace p || p `elem` ("(>|" :: String)
  | c == spanClose = Just $ CheckNext \n -> isRubyPunct n || isRubySpace n || n `elem` ("<)" :: String)
  -- The same placeholder sits on both sides of @code@. RedCloth requires a
  -- non-word character on both sides, and on each side one of the two
  -- neighbors is the '@' itself, so checking both neighbors tests the outer one.
  | c == codeEdge = Just $ CheckPrev (not . isAsciiWord)
  | c == linkEnd = Just $ CheckNext \n -> isRubySpace n || n == '<'
  | c == imageStart = Just $ CheckPrev \p -> isRubySpace p || p == '>'
  | otherwise = Nothing

-- | Zero-width separator that RedCloth accepts as a markup boundary.
boundaryGuard :: String
boundaryGuard = "<notextile></notextile>"

resolveBoundaries :: Text -> Text
resolveBoundaries = T.pack . go Nothing . T.unpack
  where
    go _ [] = []
    go prev (c : cs) = case boundaryCheck c of
      Nothing -> c : go (Just c) cs
      Just check
        | satisfied check -> go prev cs
        -- The guard ends with '>', which is what the next check sees.
        | otherwise -> boundaryGuard <> go (Just '>') cs
        where
          -- Adjacent placeholders are skipped to reach the real neighbor.
          next = find (isNothing . boundaryCheck) cs
          satisfied (CheckPrev p)
            | c == codeEdge = maybe True p prev && maybe True p next
            | otherwise = maybe True p prev
          satisfied (CheckNext p) = maybe True p next

-- | @\\s@ in Ruby regular expressions.
isRubySpace :: Char -> Bool
isRubySpace c = c `elem` (" \t\n\r\f\v" :: String)

-- | @[[:punct:]]@ in Ruby regular expressions.
isRubyPunct :: Char -> Bool
isRubyPunct c = isPunctuation c || c `elem` ("$+<=>^`|~" :: String)

-- | @\\w@ in Ruby regular expressions.
isAsciiWord :: Char -> Bool
isAsciiWord c = isAscii c && (isAlphaNum c || c == '_')

------------------------------------------------------------------------------
-- Heading anchors

-- | Anchor names Redmine assigns to the headings, keyed by the identifiers
-- pandoc assigned to them, so that @[text](#identifier)@ keeps working.
headerAnchors :: [Block] -> Map Text Text
headerAnchors blocks =
  Map.fromList . filter (not . T.null . fst) $ zip ids (numberDuplicates Map.empty anchors)
  where
    (ids, anchors) = unzip $ query headerInfo blocks
    headerInfo (Header _ (ident, _, _) ils) = [(ident, redmineAnchor (stringify ils))]
    headerInfo _ = []
    numberDuplicates _ [] = []
    numberDuplicates seen (a : as) =
      let n = Map.findWithDefault (0 :: Int) a seen + 1
          a' = if n > 1 then a <> "-" <> tshow n else a
       in a' : numberDuplicates (Map.insert a n seen) as

-- | @ApplicationHelper#sanitize_anchor_name@ of Redmine.
redmineAnchor :: Text -> Text
redmineAnchor = joinSpaces . T.filter keep . T.strip
  where
    keep c = isRubySpace c || c == '-' || isRubyWordChar c
    -- gsub(/\s+(\-+\s*)?/, '-')
    joinSpaces t = case T.break isRubySpace t of
      (a, rest)
        | T.null rest -> a
        | otherwise ->
          let afterSpaces = T.dropWhile isRubySpace rest
              (dashes, afterDashes) = T.span (== '-') afterSpaces
              remaining
                | T.null dashes = afterSpaces
                | otherwise = T.dropWhile isRubySpace afterDashes
           in a <> "-" <> joinSpaces remaining

-- | @\\p{Word}@ in Ruby regular expressions.
isRubyWordChar :: Char -> Bool
isRubyWordChar c = case generalCategory c of
  UppercaseLetter -> True
  LowercaseLetter -> True
  TitlecaseLetter -> True
  ModifierLetter -> True
  OtherLetter -> True
  NonSpacingMark -> True
  SpacingCombiningMark -> True
  EnclosingMark -> True
  DecimalNumber -> True
  LetterNumber -> True
  ConnectorPunctuation -> True
  _ -> False

------------------------------------------------------------------------------
-- Escaping

-- | Escape plain text so that RedCloth shows it as is. @forced@ lists
-- characters that must be escaped wherever they appear.
--
-- Pandoc splits text at spaces, so the start of a 'Str' is the only place
-- (besides after @(@ and @>@) where a markup mark can open.
escapeTextForTextile :: [Char] -> Text -> Text
escapeTextForTextile forced t = T.concat $ zipWith3 escapeAt [0 :: Int ..] (Nothing : map Just s) (T.tails t)
  where
    s = T.unpack t
    escapeAt i prev rest = case T.uncons rest of
      Nothing -> ""
      Just (c, after)
        | c `elem` forced -> charRef c
        | c `elem` ("|<" :: String) -> charRef c
        | c == '&' && isCharRefStart after -> charRef c
        | c == '[' && isFootnoteRef after -> charRef c
        | i == 0 && (isLineMarker || isBlockSignature) -> charRef c
        | c `elem` spanMarks && opensAfter prev && not (T.null after) -> charRef c
        | c == '@' && maybe True (not . isAsciiWord) prev -> charRef c
        | otherwise -> T.singleton c
    -- '?' and '=' open ??cite?? and ==notextile==; '!' opens !image!
    spanMarks = "*_-+^~%?=!" :: String
    opensAfter = maybe True (`elem` ("(>" :: String))
    -- "* ", "# " and "> " at the start of a line
    isLineMarker = T.all (`elem` ("*#" :: String)) t || ">" `T.isPrefixOf` t
    isBlockSignature =
      let (name, rest) = T.span isAlphaNum t
       in ( name `elem` ["h1", "h2", "h3", "h4", "h5", "h6", "p", "bq", "bc", "pre", "notextile", "table"]
              || ("fn" `T.isPrefixOf` name && T.all isDigit (T.drop 2 name) && T.length name > 2)
          )
            && maybe False ((`elem` (".({[<>=" :: String)) . fst) (T.uncons rest)
    -- RedCloth keeps "&" followed by /[#a-z0-9]+;/ as a character reference
    isCharRefStart after =
      let (body, rest) = T.span (\x -> (isAscii x && isAlphaNum x) || x == '#') after
       in not (T.null body) && ";" `T.isPrefixOf` rest
    isFootnoteRef after =
      let (digits, rest) = T.span isDigit after
       in not (T.null digits) && "]" `T.isPrefixOf` rest

charRef :: Char -> Text
charRef c = "&#" <> tshow (ord c) <> ";"

withEscapeChar :: PandocMonad m => Char -> TW m a -> TW m a
withEscapeChar c action = do
  old <- gets stEscapeChars
  modify $ \s -> s {stEscapeChars = c : old}
  result <- action
  modify $ \s -> s {stEscapeChars = old}
  return result

-- | Remove blank lines, which would end the enclosing table row, footnote
-- or definition.
removeBlankLines :: Text -> Text
removeBlankLines = T.intercalate "\n" . filter (not . T.null . T.strip) . T.lines

------------------------------------------------------------------------------
-- Blocks

-- | Convert Pandoc block element to Textile.
blockToTextile ::
  PandocMonad m =>
  -- | Options
  WriterOptions ->
  -- | Block element
  Block ->
  TW m Text
-- 利便性のための hack
-- A macro ends at the first "}}", so a collapse inside another one cannot be
-- expressed; it is expanded, with its label as a bold line.
blockToTextile opts (Div (_, ["collapse"], keyvals) bs) = do
  let mOpen = lookup "open" keyvals
      mClose = lookup "close" keyvals
  nested <- gets stInCollapse
  if nested
    then blockListToTextile opts (maybe [] (\l -> [Para [Strong [Str l]]]) mOpen <> bs)
    else collapseToTextile opts mOpen mClose bs
blockToTextile opts (Div _ bs) = blockListToTextile opts bs
blockToTextile opts (Plain inlines) =
  inlineListToTextile opts (dropTrailingBreaks inlines)
-- Redmine has no figures; the image stands as a paragraph of its own.
blockToTextile opts (Figure _ _ body) =
  blockListToTextile opts (map plainToPara body)
  where
    plainToPara (Plain inlines) = Para inlines
    plainToPara b = b
blockToTextile opts (Para inlines) = do
  listLevel <- gets stListLevel
  contents <- inlineListToTextile opts (dropTrailingBreaks inlines)
  return $ contents <> if null listLevel then "\n" else ""
blockToTextile opts (LineBlock lns) =
  blockToTextile opts $ linesToPara lns
blockToTextile _ b@(RawBlock f str)
  | f == Format "textile" = return str
  | f == Format "html" && isAllowedHtml str = return str
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
-- A diagram stays a code block where the macro could not hold it: a "}}" in
-- the diagram, or an enclosing collapse, would end the macro early.
blockToTextile _ (CodeBlock (_, classes, _) str)
  | "mermaid" `elem` classes = do
    inCollapse <- gets stInCollapse
    if inCollapse || "}}" `T.isInfixOf` str
      then codeBlockToTextile classes str
      else macroToTextile "mermaid" "" str
  | otherwise = codeBlockToTextile classes str
blockToTextile opts (BlockQuote blocks) = do
  contents <- blockListToTextile opts blocks
  return $ T.unlines $ map ("> " <>) $ T.lines contents
blockToTextile opts (Table _ blkCapt specs thead tbody tfoot) = do
  let (capt, aligns, _widths, headers, rows') = toLegacyTable blkCapt specs thead tbody tfoot
  caption <-
    if null capt
      then return ""
      else (<> "\n\n") <$> inlineListToTextile opts capt
  let cellToTextile bs = removeBlankLines . stripTrailingNewlines <$> blockListToTextile opts bs
  let cellsToRow cells = "|" <> T.intercalate "|" cells <> "|"
  hs <- mapM (fmap ("_. " <>) . cellToTextile) headers
  let header = if all null headers then "" else cellsToRow hs <> "\n"
  let blocksToCell (align, bs) = do
        contents <- cellToTextile bs
        let alignMarker = case align of
              AlignLeft -> "<. "
              AlignRight -> ">. "
              AlignCenter -> "=. "
              AlignDefault -> ""
        return $ alignMarker <> contents
  bs <- mapM (mapM blocksToCell . zip aligns) rows'
  return $ caption <> header <> T.unlines (map cellsToRow bs)
blockToTextile opts (BulletList items) = listToTextile opts '*' items
-- Redmine does not support start numbers (#3), so every list starts at 1.
blockToTextile opts (OrderedList _ items) = listToTextile opts '#' items
-- Redmine has no definition lists: the term is set in bold and the
-- definitions follow it on separate lines.
blockToTextile opts (DefinitionList items) = do
  contents <- mapM (definitionListItemToTextile opts) items
  return $ T.intercalate "\n" contents

collapseToTextile :: PandocMonad m => WriterOptions -> Maybe Text -> Maybe Text -> [Block] -> TW m Text
collapseToTextile opts mOpen mClose bs = do
  modify $ \s -> s {stInCollapse = True}
  contents <- blockListToTextile opts bs
  modify $ \s -> s {stInCollapse = False}
  let toggleMessage = case (macroArg <$> mOpen, macroArg <$> mClose) of
        (Nothing, Nothing) -> ""
        (Just open, Nothing) -> "(" <> open <> ")"
        (Nothing, Just close) -> "(" <> "表示," <> close <> ")"
        (Just open, Just close) -> "(" <> open <> "," <> close <> ")"
  macroToTextile "collapse" toggleMessage contents

-- | A block macro: @{{name(args)@, the body, and @}}@ on separate lines.
macroToTextile :: PandocMonad m => Text -> Text -> Text -> TW m Text
macroToTextile name args body = do
  listLevel <- gets stListLevel
  let macroLines = ["{{" <> name <> args, body, "}}"]
  -- Inside a list, a trailing empty line would end the list.
  return $
    if null listLevel
      then T.unlines macroLines
      else T.intercalate "\n" macroLines

codeBlockToTextile :: PandocMonad m => [Text] -> Text -> TW m Text
codeBlockToTextile classes str = do
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

-- | Redmine splits macro arguments at commas outside double quotes.
macroArg :: Text -> Text
macroArg arg
  | T.any (`elem` (",\"" :: String)) arg = "\"" <> T.concatMap doubleQuote arg <> "\""
  | otherwise = arg
  where
    doubleQuote '"' = "\"\""
    doubleQuote c = T.singleton c

-- | Turn @\<details>\<summary>label\</summary> ... \</details>@ into a
-- collapse 'Div'. Pandoc reads these tags as separate raw HTML blocks around
-- the parsed contents, and Redmine would show the tags as text.
groupDetails :: [Block] -> [Block]
groupDetails [] = []
groupDetails (b : bs)
  | isHtmlTag "<details" b
    , Just (inner, rest) <- splitAtClose (0 :: Int) [] bs =
    let (label, body) = takeSummary inner
        attrs = maybe [] (\l -> [("open", l)]) label
     in Div ("", ["collapse"], attrs) (groupDetails body) : groupDetails rest
  | otherwise = b : groupDetails bs
  where
    splitAtClose _ _ [] = Nothing
    splitAtClose depth acc (x : xs)
      | isHtmlTag "</details" x =
        if depth == 0 then Just (reverse acc, xs) else splitAtClose (depth - 1) (x : acc) xs
      | isHtmlTag "<details" x = splitAtClose (depth + 1) (x : acc) xs
      | otherwise = splitAtClose depth (x : acc) xs
    takeSummary (x : xs)
      | isHtmlTag "<summary" x
        , (label, _ : body) <- break (isHtmlTag "</summary") xs =
        (Just (T.strip (stringify label)), body)
    takeSummary xs = (Nothing, xs)

-- | Whether a block is a raw HTML tag starting with the given text, e.g.
-- @"<details"@ matches @\<details>@ and @\<details open>@.
isHtmlTag :: Text -> Block -> Bool
isHtmlTag prefix (RawBlock (Format "html") str) =
  case T.stripPrefix prefix (T.toLower (T.strip str)) of
    Just rest -> maybe False ((`elem` (" >" :: String)) . fst) (T.uncons rest)
    Nothing -> False
isHtmlTag _ _ = False

-- | A line break at the end of a block has no visible effect in Markdown,
-- but in Textile the empty line it leaves would end a list or paragraph.
dropTrailingBreaks :: [Inline] -> [Inline]
dropTrailingBreaks = reverse . dropWhile isBreak . reverse
  where
    isBreak LineBreak = True
    isBreak SoftBreak = True
    isBreak Space = True
    isBreak _ = False

-- | Whether raw HTML is one of the few tags Redmine renders as HTML.
isAllowedHtml :: Text -> Bool
isAllowedHtml str =
  case T.stripPrefix "<" (T.strip str) of
    Just rest ->
      let name = T.toLower . T.takeWhile isAlphaNum $ fromMaybe rest (T.stripPrefix "/" rest)
       in name `elem` ["pre", "code", "kbd", "notextile"]
    Nothing -> False

-- Auxiliary functions for lists:

listToTextile :: PandocMonad m => WriterOptions -> Char -> [[Block]] -> TW m Text
listToTextile opts marker items = do
  modify $ \s -> s {stListLevel = stListLevel s <> [marker]}
  level <- gets $ length . stListLevel
  contents <- mapM (listItemToTextile opts) items
  modify $ \s -> s {stListLevel = init (stListLevel s)}
  return $ T.intercalate "\n" contents <> (if level > 1 then "" else "\n")

-- | Convert bullet or ordered list item (list of blocks) to Textile.
listItemToTextile ::
  PandocMonad m =>
  WriterOptions ->
  [Block] ->
  TW m Text
listItemToTextile opts items = do
  contents <- blockListToTextile opts items
  marker <- gets stListLevel
  return $ T.pack marker <> " " <> contents

-- | Convert definition list item (label, list of blocks) to Textile.
definitionListItemToTextile ::
  PandocMonad m =>
  WriterOptions ->
  ([Inline], [[Block]]) ->
  TW m Text
definitionListItemToTextile opts (label, items) = do
  term <- inlineToTextile opts (Strong label)
  contents <- mapM (fmap removeBlankLines . blockListToTextile opts) items
  return $ T.unlines (term : contents)

-- | Convert list of Pandoc block elements to Textile.
blockListToTextile ::
  PandocMonad m =>
  -- | Options
  WriterOptions ->
  -- | List of block elements
  [Block] ->
  TW m Text
blockListToTextile opts blocks = do
  texts <- mapM (blockToTextile opts) blocks
  listLevel <- gets stListLevel
  let rendered = zip blocks texts
      -- In a list item Redmine turns a line break into <br />, which after
      -- </pre> shows as an empty line; text can follow </pre> directly.
      separator (_, prev) (next, _)
        | not (null listLevel) && "</pre>" `T.isSuffixOf` prev && isText next = ""
        | otherwise = "\n"
      isText Para {} = True
      isText Plain {} = True
      isText _ = False
  return . T.concat $
    take 1 texts <> concat (zipWith (\a b -> [separator a b, snd b]) rendered (drop 1 rendered))

------------------------------------------------------------------------------
-- Inlines

-- | Convert list of Pandoc inline elements to Textile.
inlineListToTextile ::
  PandocMonad m =>
  WriterOptions ->
  [Inline] ->
  TW m Text
inlineListToTextile opts lst =
  T.concat <$> mapM (inlineToTextile opts) lst

-- | Wrap inline markup in its delimiter. The delimiter is escaped inside the
-- contents, since RedCloth would otherwise end the markup there.
spanToTextile :: PandocMonad m => WriterOptions -> Char -> [Inline] -> TW m Text
spanToTextile opts delim lst = do
  contents <- withEscapeChar delim $ inlineListToTextile opts lst
  let d = T.singleton delim
  return $ T.singleton spanOpen <> d <> contents <> d <> T.singleton spanClose

-- | Convert Pandoc inline element to Textile.
inlineToTextile :: PandocMonad m => WriterOptions -> Inline -> TW m Text
inlineToTextile opts (Span _ lst) =
  inlineListToTextile opts lst
inlineToTextile opts (Emph lst) = spanToTextile opts '_' lst
inlineToTextile opts (Underline lst) = spanToTextile opts '+' lst
inlineToTextile opts (Strong lst) = spanToTextile opts '*' lst
inlineToTextile opts (Strikeout lst) = spanToTextile opts '-' lst
inlineToTextile opts (Superscript lst) = spanToTextile opts '^' lst
inlineToTextile opts (Subscript lst) = spanToTextile opts '~' lst
inlineToTextile opts (SmallCaps lst) = inlineListToTextile opts lst
inlineToTextile opts (Quoted SingleQuote lst) = do
  contents <- inlineListToTextile opts lst
  return $ "'" <> contents <> "'"
inlineToTextile opts (Quoted DoubleQuote lst) = do
  contents <- inlineListToTextile opts lst
  return $ "\"" <> contents <> "\""
inlineToTextile opts (Cite _ lst) = inlineListToTextile opts lst
-- Inside <code> Redmine escapes the text itself (@...@ keeps "&amp;" as a
-- character reference), and a table splits cells
-- at '|' before @code@ is recognized, so those cases use <code>.
inlineToTextile _ (Code _ str)
  | T.null str || T.any (`elem` ("@|&" :: String)) str = return $ "<code>" <> str <> "</code>"
  | otherwise = return $ T.singleton codeEdge <> "@" <> str <> "@" <> T.singleton codeEdge
inlineToTextile _ (Str str) = do
  forced <- gets stEscapeChars
  return $ escapeTextForTextile forced str
-- Redmine has no math markup; the TeX source is shown as code.
inlineToTextile _ (Math _ str) = return $ "<code>" <> str <> "</code>"
inlineToTextile _ il@(RawInline f str)
  | f == Format "textile" = return str
  | f == Format "html" && isBrTag str = return "\n"
  | f == Format "html" && isAllowedHtml str = return str
  | otherwise = do
    report $ InlineNotRendered il
    return ""
  where
    isBrTag t = T.toLower (T.filter (`notElem` (" /" :: String)) t) == "<br>"
inlineToTextile _ LineBreak = return "\n"
inlineToTextile _ SoftBreak = return " "
inlineToTextile _ Space = return " "
inlineToTextile opts (Link (_, cls, _) txt (src, tit)) = do
  anchors <- gets stAnchors
  label <- case txt of
    [Code _ s]
      | s == src -> return s
    [Str s]
      | s == src -> return s
    _ -> inlineListToTextile opts txt
  let classes =
        if null cls || cls == ["uri"] || cls == ["email"]
          then ""
          else "(" <> T.unwords cls <> ")"
      title =
        if T.null tit || T.any (`elem` ("()\"" :: String)) tit
          then ""
          else "(" <> tit <> ")"
  return $ case T.stripPrefix "#" src of
    -- Textile links accept only absolute URLs, so links within the page use
    -- Redmine's wiki link syntax.
    Just ident ->
      let anchor = Map.findWithDefault ident ident anchors
       in "[[#" <> anchor <> "|" <> label <> "]]"
    Nothing ->
      "\"" <> classes <> label <> title <> "\":" <> encodeSpaces src <> T.singleton linkEnd
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
  return $ T.singleton imageStart <> "!" <> classes <> styles <> encodeSpaces source <> txt <> "!"
inlineToTextile opts (Note contents) = do
  curNotes <- gets stNotes
  let newnum = length curNotes + 1
  contents' <- blockListToTextile opts contents
  let thisnote = "fn" <> tshow newnum <> ". " <> removeBlankLines contents' <> "\n"
  modify $ \s -> s {stNotes = thisnote : curNotes}
  return $ "[" <> tshow newnum <> "]"

-- | A space would end the URL of a Textile link or image.
encodeSpaces :: Text -> Text
encodeSpaces = T.concatMap \c -> if c == ' ' then "%20" else T.singleton c
