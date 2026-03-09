{
  description = "Nathan Gill's Haskell flake template";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      utils,
      nixpkgs,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShell = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            ghc
            ormolu
            haskell-language-server

            haskellPackages.cabal-fmt
            haskellPackages.cabal-install
          ];
        };
      }
    );
}
