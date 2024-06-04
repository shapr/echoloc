{
  description = "echoloc";

  inputs = {
    # Nix Inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
  };

  outputs = {
    self,
    nixpkgs,
  }:
    let
      forAllSystems = function:
        nixpkgs.lib.genAttrs [
          "x86_64-linux"
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
        ] (system: function rec {
          inherit system;
          compilerVersion = "ghc963";
          pkgs = nixpkgs.legacyPackages.${system};
          hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
            overrides = hfinal: hprev: {
              echoloc = hfinal.callCabal2nix "echoloc" ./. {};
            };
          };
        });
    in
    {
      # nix fmt
      formatter = forAllSystems ({pkgs, ...}: pkgs.alejandra);

      # nix develop
      devShell = forAllSystems ({hsPkgs, pkgs, ...}:
        hsPkgs.shellFor {
          # withHoogle = true;
          packages = p: [
            p.echoloc
          ];
          buildInputs = with pkgs;
            [
              hsPkgs.haskell-language-server
              haskellPackages.cabal-install
              cabal2nix
              haskellPackages.ghcid
              haskellPackages.fourmolu
              haskellPackages.cabal-fmt
              haskellPackages.OpenAL
              haskellPackages.ALUT
            ]
            ++ (builtins.attrValues (import ./scripts.nix {s = pkgs.writeShellScriptBin;}));
        });

      # nix build
      packages = forAllSystems ({hsPkgs, ...}: {
          echoloc = hsPkgs.echoloc;
          default = hsPkgs.echoloc;
      });

      # You can't build the echoloc package as a check because of IFD in cabal2nix
      checks = {};

      # nix run
      apps = forAllSystems ({system, ...}: {
        echoloc = {
          type = "app";
          program = "${self.packages.${system}.echoloc}/bin/echoloc";
        };
        default = self.apps.${system}.echoloc;
      });
    };
}
