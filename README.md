# pandoc-md2redmine

Markdown to Redmine Textile converter using Pandoc.

## Overview

This tool converts Markdown documents to Redmine's Textile format. It's particularly useful for:

- Converting documentation from Markdown to Redmine wiki format
- Maintaining documentation in Markdown while publishing to Redmine
- Automating wiki page updates

## Installation

```bash
# Using Nix
nix profile install .

# Using Cabal
cabal install
```

## Usage

```bash
# Convert from stdin
pandoc-md2redmine < input.md > output.textile

# Convert from file
pandoc-md2redmine input.md > output.textile
```

