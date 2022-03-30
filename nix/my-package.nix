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
  haskPkgs = pkgs.haskell.packages.${compiler};
  drv = haskPkgs.callCabal2nix "pandoc-md2redmine" src { };
in
drv
