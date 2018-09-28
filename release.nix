let
  config = {
    packageOverrides = pkgs: rec {

      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          clash-ghc = haskellPackagesNew.callPackage ./nix/clash-ghc.nix { };
          clash-lib = haskellPackagesNew.callPackage ./nix/clash-lib.nix { };
          clash-prelude = pkgs.haskell.lib.disableCabalFlag (
            haskellPackagesNew.callPackage ./nix/clash-prelude.nix { }
          ) "doctests";

          zoid = haskellPackagesNew.callPackage ./default.nix { };
        };
      };

    }; 
  };


  pkgs = import <unstable> { inherit config; };
  
in
  { zoid = pkgs.lib.overrideDerivation pkgs.haskellPackages.zoid (drv: rec {
      configurePhase = ''
        runHook preConfigure
        export GHC_PACKAGE_PATH="$packageConfDir:"
        runHook postConfigure
      '';
      setupCommand = "./Setup"; 
      buildPhase = ''
        runHook preBuild
        ${setupCommand} -j$NIX_BUILD_CORES
        runHook postBuild
      '';
      installPhase = ''
        mkdir $out
        cp build/Zoid.v $out
      '';
    });
  }
