{
  description = "nls";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      imports = [
        inputs.haskell-flake.flakeModule
      ];
      perSystem =
        {
          self',
          system,
          lib,
          config,
          pkgs,
          ...
        }:
        {
          haskellProjects.default = {
            autoWire = [ "packages" ];
          };

          devShells.default = pkgs.mkShell {
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];

            nativeBuildInputs = with pkgs; [
              haskellPackages.cabal-fmt
            ];
          };
        };
    };
}
