{ mkDerivation, array, base, Cabal, containers, directory, filepath
, HUnit, lib, optparse-applicative, parsec, process, QuickCheck
, test-framework, test-framework-hunit, test-framework-quickcheck2
, time, transformers, stdenv
}:
mkDerivation {
  pname = "acov";
  version = "0.2.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  setupHaskellDepends = [ base Cabal directory process ];
  libraryHaskellDepends = [
    array base containers parsec transformers
  ];
  executableHaskellDepends = [
    array base containers directory filepath optparse-applicative
    parsec time
  ];
  testHaskellDepends = [
    base containers HUnit parsec QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2
  ];
  description = "A SystemVerilog functional coverage generator";
  license = lib.licenses.bsd3;
}
