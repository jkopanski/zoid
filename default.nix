{ stdenv, lib, haskellPackages
, ghc, ghcCommand, ghcCommandCaps, glibcLocales
}:
stdenv.mkDerivation rec {
  name = "zoid";
  version = "0.0.1.0";
  src = lib.cleanSourceWith {
    filter = n: t:
      n != "build" &&
      n != "result" &&
      t != "unknown";
    src = lib.cleanSource ./.;
  };
  nativeBuildInputs = [
    haskellPackages.ghcEnv
  ];
  env = with haskellPackages; stdenv.mkDerivation {
    name = "interactive-${name}-${version}-environment";
    nativeBuildInputs = [ ghcEnv ] ++ nativeBuildInputs;
    LANG = "en_US.UTF-8";
    LOCALE_ARCHIVE = lib.optionalString (stdenv.hostPlatform.libc == "glibc") "${glibcLocales}/lib/locale/locale-archive";
    shellHook = ''
      export NIX_${ghcCommandCaps}="${ghcEnv}/bin/${ghcCommand}"
      export NIX_${ghcCommandCaps}PKG="${ghcEnv}/bin/${ghcCommand}-pkg"
      export NIX_${ghcCommandCaps}_DOCDIR="${ghcEnv}/share/doc/ghc/html"
      ${shellHook}
    '';
  };
  preBuild = shellHook;
  buildPhase = ''
    ./Make.hs -j$NIX_BUILD_CORES
  '';
  installPhase = ''
    mkdir -p $out
    cp build/chip.bin "$out"
  '';
  LANG = "pl_PL.UTF-8";
  shellHook = ''
    export NIX_${ghcCommandCaps}="${haskellPackages.ghcEnv}/bin/${ghcCommand}"
    export NIX_${ghcCommandCaps}PKG="${haskellPackages.ghcEnv}/bin/ghc-pkg"
    export NIX_${ghcCommandCaps}_DOCDIR="${haskellPackages.ghcEnv}/share/doc/ghc/html"
    export NIX_${ghcCommandCaps}_LIBDIR="${haskellPackages.ghcEnv}/lib/${ghcCommand}-${ghc.version}"
  '';
}
