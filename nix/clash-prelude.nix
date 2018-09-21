{ mkDerivation, array, base, bifunctors, constraints, criterion
, data-binary-ieee754, data-default, deepseq, doctest, ghc-prim
, ghc-typelits-extra, ghc-typelits-knownnat
, ghc-typelits-natnormalise, half, integer-gmp, lens, QuickCheck
, reflection, singletons, stdenv, template-haskell, transformers
, vector
}:
mkDerivation {
  pname = "clash-prelude";
  version = "0.99.3";
  sha256 = "ef4628671f207dc7f27c7df46e5151f4c20826438507defd9bf05f76658b77bc";
  libraryHaskellDepends = [
    array base bifunctors constraints data-binary-ieee754 data-default
    deepseq ghc-prim ghc-typelits-extra ghc-typelits-knownnat
    ghc-typelits-natnormalise half integer-gmp lens QuickCheck
    reflection singletons template-haskell transformers vector
  ];
  testHaskellDepends = [ base doctest ];
  benchmarkHaskellDepends = [
    base criterion deepseq template-haskell
  ];
  homepage = "http://www.clash-lang.org/";
  description = "CAES Language for Synchronous Hardware - Prelude library";
  license = stdenv.lib.licenses.bsd2;
}
