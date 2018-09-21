let
  haskellBuildInputs = haskellPackages: with haskellPackages; [
    clash-ghc
    shake
  ];

  config = {
    packageOverrides = pkgs: rec {

      ghcCommand = "ghc";
      ghcCommandCaps = pkgs.lib.toUpper ghcCommand;

      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          clash-ghc = haskellPackagesNew.callPackage ./nix/clash-ghc.nix { };
          clash-lib = haskellPackagesNew.callPackage ./nix/clash-lib.nix { };
          clash-prelude = pkgs.haskell.lib.disableCabalFlag (
            haskellPackagesNew.callPackage ./nix/clash-prelude.nix { }
          ) "doctests";

          ghcEnv = haskellPackagesNew.ghcWithHoogle haskellBuildInputs;
        };
      };

      zoid = pkgs.callPackage ./default.nix { };
    }; 
  };


  pkgs = import <unstable> { inherit config; };
  
in
  { zoid = pkgs.zoid;
  }
