{ mkDerivation, stdenv
, clash-ghc, clash-prelude, clash-lib
, shake
, yosys
# yosys is missing graphviz dep
, graphviz-nox
}:
mkDerivation rec {
  pname = "zoid";
  version = "0.0.1.0";
  src = ./.;
  isExecutable = true;
  buildDepends = [ clash-ghc clash-prelude clash-lib ];
  buildTools = [
    shake
    yosys
    graphviz-nox
  ];
  executableHaskellDepends = [ clash-prelude clash-lib ];
  license = stdenv.lib.licenses.bsd3;
}
