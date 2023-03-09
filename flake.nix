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

      supported-ghc-versions = [
        "ghc922"
        "ghc902"
        "ghc8107"
        "ghc884"
      ];
      default-ghc-version = "ghc922";
      per-compiler = fkey: fattr:
        (builtins.listToAttrs (builtins.map (str: { name = fkey str; value = fattr str; }) supported-ghc-versions))
        // { default = fattr default-ghc-version; };

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
        {
          devShells = per-compiler (str: str + "-shell") mkShell;
          packages.calligraphy = pkgs.calligraphy;
          defaultPackage = pkgs.calligraphy;
          apps = per-compiler (str: "calligraphy-" + str) mkApp;
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
