# Licenses of the distributed pandoc-md2redmine executable

The `pandoc-md2redmine` executable in this archive is a self-extracting
bundle. It contains the program, which statically links the GHC runtime and
every Haskell library it uses, and the shared libraries the program loads
(glibc, GMP, libffi and zlib).

## Terms for the executable as a whole

pandoc-md2redmine itself is licensed under the GNU General Public License,
version 2 or (at your option) any later version. So are pandoc, HsYAML,
texmath and skylighting, which it links.

skylighting compiles in syntax definitions taken from KDE. One of them,
the definition for R, declares its license as "GPLv2" without stating
whether later versions are allowed. The executable as a whole is therefore
distributed under the terms of the GNU General Public License, version 2.
The other components keep their own licenses, which are all compatible
with GPLv2.

## Contents of this directory

Each subdirectory holds the notice files of one component, copied
unchanged from that component's source archive.

| Directory  | Components |
|------------|------------|
| `self/`    | pandoc-md2redmine |
| `ghc/`     | GHC's runtime system and boot libraries (`libraries/*/LICENSE`) |
| `haskell/` | Haskell libraries from Hackage |
| `native/`  | Shared libraries carried in the bundle |

Two components also embed data under licenses of their own. Their
directories contain an extra listing:

- `haskell/skylighting-core-*/SYNTAX-DEFINITIONS.txt` gives the author and
  declared license of each syntax definition.
- `haskell/citeproc-*/CSL-LOCALES.txt` gives the rights statement and
  translators of each CSL locale file (CC BY-SA 3.0).

## Corresponding source

The release that provides this archive also provides
`*-sources.tar`. It contains the source archive of every component above
and the patches applied to it when building. The file `*-components.json`
lists each component with the SHA-256 of its source archive and patches.

The license field in `*-components.json` is taken from the nixpkgs package
metadata and can be less precise than the component's own notices. For
example, nixpkgs reports HsYAML as GPL-2.0-only, while HsYAML's own
`.cabal` file declares GPL-2.0-or-later. The notice files in this
directory are authoritative.

The build is described by `flake.nix` and `flake.lock` in the
pandoc-md2redmine source archive. `flake.lock` pins the nixpkgs revision
that supplies the build scripts of every component. The bundled shared
libraries can be replaced: `pandoc-md2redmine --extract DIR` unpacks the
bundle, and the libraries are in `DIR/lib`.
