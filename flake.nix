{
  description = "calligraphy";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.unstable.url = "github:NixOS/nixpkgs/haskell-updates";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: _: {
          hsPkgs =
            self.haskell-nix.project' rec {
              src = ./.;
              compiler-nix-name = "ghc8107";
              shell = {
                tools = {
                  cabal = { };
                  ghcid = { };
                  haskell-language-server = { };
                  hlint = { };
                  ormolu = {
                    version = "latest";
                    modules = [ ({ lib, ... }: { options.nonReinstallablePkgs = lib.mkOption { apply = lib.remove "Cabal"; }; }) ];
                  };
                };
                buildInputs = [ pkgs.graphviz ];
              };
            };
        };
        pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            inputs.haskellNix.overlay
            overlay
          ];
        };
        flake = pkgs.hsPkgs.flake { };
        unstable = import inputs.unstable { inherit system; };
      in
      rec {
        devShells = {
          stack-nightly-shell = unstable.mkShell {
            packages = [
              unstable.stack
              unstable.haskell.compiler.ghc922
            ];
            STACK_YAML = "stack-nightly.yaml";
          };
          stack-lts19-shell = unstable.mkShell {
            packages = [
              unstable.stack
              unstable.haskell.compiler.ghc902
            ];
            STACK_YAML = "stack-lts19.yaml";
          };
          haskell-nix-shell = flake.devShell;
        };

        devShell = devShells.haskell-nix-shell;

        defaultPackage = flake.packages."calligraphy:exe:calligraphy";

      });
}
