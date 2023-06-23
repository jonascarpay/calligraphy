{
  description = "calligraphy";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";
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

          per-compiler = f: pkgs.lib.genAttrs [
            "ghc962"
            "ghc945"
            "ghc927"
            "ghc902"
            "ghc8107"
            "ghc884"
          ]
            (ghc: f pkgs.haskell.packages.${ghc});

          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };

          mkApp = hspkgs: pkgs.haskell.lib.compose.justStaticExecutables hspkgs.calligraphy;

          mkBuildShell = hspkgs:
            hspkgs.shellFor {
              packages = hpkgs: [ hpkgs.calligraphy ];
              nativeBuildInputs = [ hspkgs.cabal-install ];
            };

          mkDevShell = hspkgs:
            hspkgs.shellFor {
              withHoogle = true;
              packages = hpkgs: [ hpkgs.calligraphy ];
              nativeBuildInputs = [
                hspkgs.cabal-install
                hspkgs.ormolu
                pkgs.bashInteractive
                pkgs.graphviz
                hspkgs.hlint
                hspkgs.haskell-language-server
              ];
            };

        in
        {
          packages.default = pkgs.calligraphy;

          apps.default = pkgs.calligraphy;
          apps.calligraphyFor = per-compiler mkApp;

          devShells.default = mkDevShell pkgs.haskellPackages;
          devShells.build-shells = per-compiler mkBuildShell;
          devShells.dev-shells = per-compiler mkDevShell;
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}
