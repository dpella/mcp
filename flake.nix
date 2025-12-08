{
  # This is a template created by `hix init`
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    # Core inputs for the entire build system
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05-small";
    haskellNix = {
      url = "github:plow-technologies/haskell.nix/ghc966_armv7l_fixes";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-2405.follows = "nixpkgs";
    };
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
    in
      flake-utils.lib.eachSystem supportedSystems (system:
      let
        overlays = [ haskellNix.overlay
          (final: prev: {
            hixProject =
              final.haskell-nix.hix.project {
                src = ./.;
                evalSystem = "x86_64-linux";
              };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.hixProject.flake {};
      in flake // {
        legacyPackages = pkgs;

        packages = flake.packages // { default = flake.packages."mcp:exe:mcp-http"; };
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use the IOG nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = ["https://cache.iog.io"];
    extra-trusted-public-keys = ["hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="];
    allow-import-from-derivation = "true";
  };
}
