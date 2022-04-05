{
  description = "calligraphy";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              calligraphy = hfinal.callCabal2nix "calligraphy" ./. { };
            };
        };
        calligraphy = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.calligraphy;
        calligraphy-shell = final.haskellPackages.shellFor {
          withHoogle = false;
          packages = hpkgs: [ hpkgs.calligraphy ];
          nativeBuildInputs = [
            final.cabal-install
            final.ghcid
            final.haskellPackages.haskell-language-server
            final.hlint
            final.ormolu
            final.bashInteractive # see: https://discourse.nixos.org/t/interactive-bash-with-nix-develop-flake/15486
          ];
        };
      };

      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
        in
        rec {
          devShells = {
            stack-nightly-shell = pkgs.mkShell {
              packages = [
                pkgs.stack
                pkgs.haskell.compiler.ghc922
              ];
              STACK_YAML = "stack-nightly.yaml";
            };
            stack-lts19-shell = pkgs.mkShell {
              packages = [
                pkgs.stack
                pkgs.haskell.compiler.ghc902
              ];
              STACK_YAML = "stack-lts19.yaml";
            };
            cabal-shell = pkgs.calligraphy-shell;
          };
          devShell = devShells.cabal-shell;
          packages.calligraphy = pkgs.calligraphy;
          defaultPackage = pkgs.calligraphy;
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
