# AGENTS.md - Developer Guide for AI Agents

## Project Overview

`pandoc-md2redmine` is a Markdown to Redmine Textile converter built with Haskell and Pandoc. The project converts Markdown documents to Redmine's wiki format (Textile).

**Key Technologies:**
- Haskell with GHC 8.10.7
- Pandoc for document conversion
- Hspec + hspec-golden for testing
- Cabal for build management

## Testing

### Running Tests

```bash
make test
```

This command builds the test suite and runs all tests with detailed output.

### Test Framework

The project uses **golden tests** with `hspec-golden`. Golden tests compare actual output against expected "golden" files stored in `test/cases/`.

**Benefits of golden tests:**
- Clear diff output when tests fail
- Automatic generation of `.actual` files for comparison
- Easy to review and update expected outputs

### Adding New Test Cases

1. **Create a test input file** in `test/cases/`:
   ````bash
   cat > test/cases/case04/input.md << 'EOF'
   # Your test markdown here
   * List item with code:
     ```
     code here
     ```
   EOF
   ````

2. **Generate the golden file**:
   ```bash
   cabal run -v0 pandoc-md2redmine < test/cases/case04/input.md > test/cases/case04/golden
   ```

3. **List the case in `test/cases/cases.json`**, under the group it belongs to.
   The spec generates one test per entry, named `case04: <description>`:
   ```json
   { "name": "case04", "description": "should describe what this tests" }
   ```
   A test fails if a directory under `test/cases/` is missing from this file.

4. **Run tests to verify**:
   ```bash
   make test
   ```

5. **Take the screenshots** with `make screenshots` (see below).

### When Tests Fail

When a golden test fails, `hspec-golden` will:
1. Display a clear diff showing expected vs actual output
2. Create a `.actual` file (e.g., `test/cases/case01/actual`)
3. Show exactly which characters differ

**To review failures:**
```bash
# Compare files
diff test/cases/caseXX/golden test/cases/caseXX/actual
```

**To update golden files** (after confirming the new output is correct):
```bash
cp test/cases/caseXX/actual test/cases/caseXX/golden
```

### Screenshots of the golden files

Each `test/cases/caseXX/redmica.png` shows how RedMica renders that case's
`golden`, and `test/cases/README.md` lists them all with the descriptions from
`cases.json`. **Whenever you add or update a golden file, or change
`cases.json`, retake the screenshots** and commit them with it:

```bash
make screenshots
```

This needs only Docker. It starts RedMica 3.1.7 with RedMica UI Extension
v0.6.0 (which renders the `{{mermaid}}` macro), publishes every golden file as
a wiki page in Textile, and takes the screenshots with Playwright. Look at the
screenshots of the cases you changed to confirm that Redmine renders them as
intended. RedMica 3.1.7 itself does not render Mermaid, so mermaid code blocks
(`<pre><code class="mermaid">`) stay code blocks in the screenshots.

## Code Structure

```
lib/
  Text/
    Pandoc/
      Md2Redmine.hs                    # Main conversion logic
      Writers/
        Textile/
          Redmine.hs                   # Redmine Textile writer

test/
  cases/                               # Golden test input/output files
    case01/input.md                    # Test input
    case01/golden                      # Expected output
    case01/actual                      # Generated when test fails
    case01/redmica.png                 # Screenshot of golden on RedMica
    cases.json                         # Names and descriptions of the cases
    README.md                          # Generated list of the screenshots
  screenshot/                          # Docker setup for `make screenshots`
  spec/
    Text/Pandoc/Writers/Textile/
      RedmineSpec.hs                   # Test specifications
```

## Key Implementation Details

### List Context Tracking

The main fix implemented tracks whether code blocks appear inside lists using the `stListLevel` state:

```haskell
blockToTextile _ (CodeBlock (_, classes, _) str) = do
  listLevel <- gets stListLevel
  let baseLines = ["<pre>" <> codeOpen, str, codeClose <> "</pre>"]
  return $
    if null listLevel
      then T.unlines (baseLines <> [""])  -- Add blank line outside lists
      else T.intercalate "\n" baseLines    -- No trailing newline in lists
```

This ensures:
- Code blocks in lists don't break list continuity
- Code blocks outside lists maintain proper spacing with following elements

## Common Development Tasks

### Build the project
```bash
make build
# or
cabal build
```

### Run a specific test
```bash
cabal test --test-options="--match '/case01/'"
```

### Clean build artifacts
```bash
make clean
```

### Generate documentation
```bash
make doc
```

## Tips for AI Agents

1. **Always run `make test` after making changes** to the conversion logic
2. **Use golden tests** - they're more reliable than inline assertions for output verification
3. **Check `.actual` files** when tests fail to understand what changed
4. **The `stListLevel` state** is crucial for tracking list context - don't modify it without understanding the implications
5. **Textile output must be exact** - whitespace matters in Redmine rendering
