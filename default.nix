{ mkDerivation, stdenv
, clash-ghc, clash-prelude, clash-lib
, shake
}:
mkDerivation rec {
  pname = "zoid";
  version = "0.0.1.0";
  src = ./.;
  isExecutable = true;
  buildDepends = [ clash-ghc clash-prelude clash-lib ];
  buildTools = [ shake ];
  executableHaskellDepends = [ clash-prelude clash-lib ];
  license = stdenv.lib.licenses.bsd3;
}
