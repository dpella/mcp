{pkgs, ...}: {
  # name = "project-name";
  compiler-nix-name = "ghc966"; # Version of GHC to use

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  shell.tools.hlint = "latest";
  # shell.tools.haskell-language-server = "latest";
}
