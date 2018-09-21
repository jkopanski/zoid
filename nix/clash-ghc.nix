{ mkDerivation, array, base, bifunctors, bytestring, clash-lib
, clash-prelude, concurrent-supply, containers, deepseq, directory
, filepath, ghc, ghc-boot, ghc-prim, ghc-typelits-extra
, ghc-typelits-knownnat, ghc-typelits-natnormalise, ghci, hashable
, haskeline, integer-gmp, lens, mtl, primitive, process, reflection
, stdenv, text, time, transformers, unbound-generics, uniplate
, unix, unordered-containers, vector
}:
mkDerivation {
  pname = "clash-ghc";
  version = "0.99.3";
  sha256 = "89470f1e133514a488d8c77ffd63535ffae5b014fc2288c6bc8879c10ddc4b3e";
  revision = "1";
  editedCabalFile = "0r86409nh73wcychmwh2j3qnbbjk9z2yxr5amx7gpc8gar4qb3qi";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bifunctors bytestring clash-lib clash-prelude
    concurrent-supply containers deepseq directory filepath ghc
    ghc-boot ghc-prim ghc-typelits-extra ghc-typelits-knownnat
    ghc-typelits-natnormalise ghci hashable haskeline integer-gmp lens
    mtl primitive process reflection text time transformers
    unbound-generics uniplate unix unordered-containers vector
  ];
  executableHaskellDepends = [ base ];
  homepage = "http://www.clash-lang.org/";
  description = "CAES Language for Synchronous Hardware";
  license = stdenv.lib.licenses.bsd2;
}
