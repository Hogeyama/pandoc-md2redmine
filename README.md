# pandoc-md2redmine

Markdown to Redmine Textile converter using Pandoc.

## Overview

This tool converts Markdown documents to Redmine's Textile format. It's particularly useful for:

- Converting documentation from Markdown to Redmine wiki format
- Maintaining documentation in Markdown while publishing to Redmine
- Automating wiki page updates

## Installation

### GitHub Releases

Each release provides a self-extracting executable for x86_64-linux. The
archive also contains `licenses/`, the license notices of everything the
executable includes.

Run directly, the executable unpacks itself into a temporary directory
on every run, which takes about a second. Unpack it once with
`--extract` and use the program it installs instead:

```bash
tmp=$(mktemp -d)
gh release download --repo Hogeyama/pandoc-md2redmine \
  --pattern 'pandoc-md2redmine-*_x86_64-linux.tar.gz' -O - | tar xz -C "$tmp"
"$tmp"/pandoc-md2redmine-*/pandoc-md2redmine --extract ~/.local/share/pandoc-md2redmine
ln -s ~/.local/share/pandoc-md2redmine/bin/pandoc-md2redmine ~/.local/bin/pandoc-md2redmine
```

`--extract` refuses an existing directory. To update, remove
`~/.local/share/pandoc-md2redmine` first and run the commands again.

### From source

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

## License

pandoc-md2redmine is licensed under the GNU General Public License, version 2
or later. See [LICENSE](LICENSE).

The executable published on GitHub Releases links pandoc and many other
libraries, and is distributed as a whole under the terms of GPL version 2.
`licenses/README.md` in the release archive explains why, and the
`*-sources.tar` asset of the same release contains the corresponding source.
