{ mkDerivation, aeson, ansi-wl-pprint, attoparsec, base, bytestring
, clash-prelude, concurrent-supply, containers, data-binary-ieee754
, deepseq, directory, errors, fgl, filepath, ghc, hashable
, integer-gmp, lens, mtl, parsers, prettyprinter, primitive
, process, reducers, stdenv, template-haskell, text, time
, transformers, trifecta, unbound-generics, unordered-containers
, vector
}:
mkDerivation {
  pname = "clash-lib";
  version = "0.99.3";
  sha256 = "556b0fc0166f2f3a48e3f9d6deecc25e472cce5183887fb046c0642cae73daba";
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson ansi-wl-pprint attoparsec base bytestring clash-prelude
    concurrent-supply containers data-binary-ieee754 deepseq directory
    errors fgl filepath ghc hashable integer-gmp lens mtl parsers
    prettyprinter primitive process reducers template-haskell text time
    transformers trifecta unbound-generics unordered-containers vector
  ];
  homepage = "http://www.clash-lang.org/";
  description = "CAES Language for Synchronous Hardware - As a Library";
  license = stdenv.lib.licenses.bsd2;
}
