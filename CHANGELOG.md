# Revision history for pandoc-md2redmine

## Unreleased

### Changed

- Markdown input recognizes headings, block quotes and lists without a preceding blank line
- A line break between Japanese characters no longer becomes a space
- `--` and quotes are kept as typed instead of becoming typographic punctuation
- Ordered lists no longer carry a start number, which Redmine does not support
- Definition lists are rendered as a bold term followed by its definitions
- Raw HTML that Redmine would show as text (comments, `<div>`, `<span>`, ...) is dropped; `<br>` becomes a line break
- Links to headings in the same page use Redmine's `[[#anchor|text]]` syntax with Redmine's anchor names
- `<details><summary>label</summary> ... </details>` becomes `{{collapse(label) ... }}`; a collapse nested in another is expanded with its label in bold, since Redmine cannot nest macros

### Fixed

- Inline markup next to Japanese text (e.g. `これは**重要**です`) is now rendered by Redmine
- Characters that Redmine would take as markup (`*`, `_`, `-`, `|`, `@`, `h1.` at line start, ...) are escaped
- Delimiters inside markup (`**a\*b**`, `*snake_case*`) no longer produce HTML tags shown as text
- Inline code containing `@`, `|` or `&`, and math, use `<code>`
- Captioned tables, including grid and multiline tables, are emitted as Textile tables instead of HTML `<table>`; multi-paragraph cells stay in one row
- A figure is no longer joined with the following paragraph
- Footnotes with several paragraphs stay in the footnote
- Autolinks no longer carry a `(uri)` class; link titles are emitted
- `-o` writes to the given file and `-f` selects the input format
- Multiple input files are joined with a newline between them
- A hard line break (two trailing spaces) at the end of a list item no longer splits the list
- A collapse inside a list no longer ends the list; labels containing commas or quotes are quoted

## 0.1.0.1 -- 2025-12-19

### Fixed

- Fixed code blocks in lists adding extra blank lines that broke list continuity
  - Code blocks inside list items no longer add trailing newlines
  - Code blocks outside lists maintain proper spacing with following elements
