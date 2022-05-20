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
      };

      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          mkApp = compiler:
             pkgs.haskell.lib.compose.justStaticExecutables
               pkgs.haskell.packages."${compiler}".calligraphy;
          mkShell = compiler:
            let hspkgs = pkgs.haskell.packages.${compiler}; in
            hspkgs.shellFor {
              withHoogle = true;
              packages = hpkgs: [ hpkgs.calligraphy ];
              GHC_VERSION = compiler;
              nativeBuildInputs = [
                hspkgs.cabal-install
                hspkgs.haskell-language-server
                hspkgs.hlint
                hspkgs.ormolu
                pkgs.bashInteractive # see: https://discourse.nixos.org/t/interactive-bash-with-nix-develop-flake/15486
                pkgs.graphviz
              ];
            };
        in
        rec {
          devShells = {
            ghc922-shell = mkShell "ghc922";
            ghc902-shell = mkShell "ghc902";
            ghc8107-shell = mkShell "ghc8107";
            ghc884-shell = mkShell "ghc884";
          };
          devShell = devShells.ghc922-shell;
          packages.calligraphy = pkgs.calligraphy;
          defaultPackage = pkgs.calligraphy;

          apps = {
            calligraphy-ghc922 = mkApp "ghc922";
            calligraphy-ghc902 = mkApp "ghc902";
            calligraphy-ghc8107 = mkApp "ghc8107";
            calligraphy-ghc884 = mkApp "ghc884";
          };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
