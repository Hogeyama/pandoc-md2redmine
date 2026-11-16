{
  description = "TODO";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils/master";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nix-bundle-elf = {
      url = "github:Hogeyama/nix-bundle-elf";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, nix-bundle-elf, ... }:
    let
      compiler = "ghc910";
      supportedSystems = [ "x86_64-linux" ];

      outputs-overlay = pkgs: prev: {
        my-package = import ./nix/my-package.nix { inherit pkgs compiler; };
        my-shell = import ./nix/my-shell.nix { inherit pkgs compiler; };
      };
    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ outputs-overlay ];
        };
      in
      {
        packages = {
          default = pkgs.my-package;
          bundled = nix-bundle-elf.lib.${system}.single-exe {
            name = "pandoc-md2redmine";
            target = "${pkgs.my-package}/bin/pandoc-md2redmine";
          };
          release = import ./nix/release {
            inherit pkgs;
            package = pkgs.my-package;
            bundled = self.packages.${system}.bundled;
            bundler = nix-bundle-elf;
            src = self;
          };
        };
        devShells = {
          default = pkgs.my-shell;
        };
      }
    );
}
