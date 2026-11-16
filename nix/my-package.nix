{ pkgs
, compiler
}:
let
  src = pkgs.lib.sourceByRegex ../. [
    "app"
    "app/.*"
    "lib"
    "lib/.*"
    "test"
    "test/.*"
    "benchmark"
    "benchmark/.*"
    "Setup.hs"
    "pandoc-md2redmine.cabal"
    "README.md"
    "CHANGELOG.md"
    "LICENSE"
  ];
  hlib = pkgs.haskell.lib.compose;
  haskPkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      # nixpkgs builds hslua against the system liblua, which links GNU
      # readline (GPL-3.0-or-later). The Lua 5.3 copy bundled with hslua has
      # no such dependency, so the distributed binary carries no readline.
      # Without -fsystem-lua, hslua compiles its bundled Lua; the system Lua
      # is the only entry nixpkgs puts in librarySystemDepends.
      hslua = hlib.overrideCabal
        (drv: {
          configureFlags =
            builtins.filter (f: f != "-fsystem-lua") (drv.configureFlags or [ ]);
          librarySystemDepends = [ ];
        })
        super.hslua;
    };
  };
  drv = haskPkgs.callCabal2nix "pandoc-md2redmine" src { };
in
drv
